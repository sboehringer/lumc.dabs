#
#	RsystemExt.R
#Wed Jan  8 10:54:20 CET 2020
# split off of Rsystem to allow simpler pullIns into other packages

#
#	<par> file handling
#

write.csvs = function(t, path, semAppend = "-sem", ...) {
	s = splitPath(path);
	write.csv(t, path);
	pathSem = sprintf("%s%s.%s", s$fullbase, semAppend, s$ext);
	# make sure t is a data.frame or dec option will not take effect <A>
	#write.csv2(t, pathSem);
	write.table(t, file = pathSem, row.names = F, col.names = T, dec = ",", sep = ";");
}

#
#	<p> file manipulation
#

#
#	command argument handling
#

#
#	<p> extended system call
#


ipAddress = function(interface = "eth0") {
	o = System(sprintf("/sbin/ifconfig %s", interface), logLevel = 6, return.output = T);
	ip = fetchRegexpr("(?<=inet addr:)[^ ]+", o$output);
	ip
}

#
#	<p> io
#

#
#	<p> cluster abstraction
#
# Example:
#specifyCluster(localNodes = 8, sourceFiles = c('RgenericAll.R', 'dataPreparation.R'));
#.clRunLocal = F;
#data.frame.types(clapply(l, f, arg1 = 1), rbind = T, do.transpose = T);

# default cluster configuration
.defaultClusterConfig = list(
	hosts = list(list(host = "localhost", count = 2, type = "PSOCK")), local = F,
	provideChunkArgument = F, reverseEvaluationOrder = T, splitN = 4, reuseCluster = F,
	nestingLevel = 0,	# records the nesting of clapply calls
	splittingLevel = 1,	# specifies at which level clapply should parallelize
	evalEnvironment = F	# call environment_eval on function before passing on
);
Snow_cluster_env__ = new.env();
specifyCluster = function(localNodes = 8, sourceFiles = NULL, cfgDict = list(), hosts = NULL,
	.doSourceLocally = F, .doCopy = T, splitN = NULL, reuseCluster = F, libraries = NULL,
	evalEnvironment = F, varsEnv = NULL, doSinkOutput = NULL, jobInit = NULL) {
	#<!> might not be available/outdated
	Require('parallel');
	cfg = merge.lists(.defaultClusterConfig,
		cfgDict,
		list(splitN = splitN, reuseCluster = reuseCluster, evalEnvironment = evalEnvironment),
		list(local = F, source = sourceFiles, libraries = libraries, varsEnv = varsEnv,
			doSinkOutput = doSinkOutput,
			jobInit = jobInit,
			hosts = (if(is.null(hosts))
			list(list(host = "localhost", count = localNodes, type = "PSOCK",
				environment = list(setwd = getwd()))) else
				hosts)
	));
	assign(".globalClusterSpecification", cfg, envir = Snow_cluster_env__);
	.globalClusterSpecification = get('.globalClusterSpecification', envir = Snow_cluster_env__);
	if (.doCopy) {
		for (h in .globalClusterSpecification$hosts) {
			if (h$host != "localhost" & !is.null(h$env$setwd)) {
				System(sprintf("ssh %s mkdir '%s' 2>/dev/null", h$host, h$env$setwd), 5);
				System(sprintf("scp '%s' %s:'%s' >/dev/null", paste(sourceFiles, collapse = "' '"),
					h$host, h$env$setwd), 5);
			}
		}
	}
	if (.doSourceLocally) {
		sourceFiles = setdiff(sourceFiles, "RgenericAll.R");	# assume we have been sourced
		eval(parse(text =
			paste(sapply(sourceFiles, function(s)sprintf("source('%s', chdir = TRUE);", s)), collapse = "")));
	}
}

# l: list, f: function, c: config
# <i><!> test clCfg$reverseEvaluationOrder before uncommenting
clapply_cluster = function(l, .f, ..., clCfg = NULL) {
	#if (clCfg$reverseEvaluationOrder) l = rev(l);

	# only support SOCK type right now <!><i>
	hosts = as.vector(unlist(sapply(clCfg$hosts, function(h){
		if (h$type == "PSOCK") rep(h$host, h$count) else NULL})));
	master = ifelse(all(hosts == "localhost"), "localhost", ipAddress("eth0"));
	establishEnvironment = T;
	cl = if (clCfg$reuseCluster) {
		if (!exists(".globalClusterObject")) {
			assign(".globalClusterObject", makeCluster(hosts, type = "PSOCK", master = master),
				envir = Snow_cluster_env__);
		} else establishEnvironment = FALSE;
		get('.globalClusterObject', envir = Snow_cluster_env__)
	} else makeCluster(hosts, type = "PSOCK", master = master);
	#clusterSetupRNG(cl);	# snow
	clusterSetRNGStream(cl, iseed = NULL);	# parallel

	clusterExport(cl, clCfg[['vars']]);

	# <p> establish node environment
	envs = listKeyValue(list.key(clCfg$hosts, "host"), list.key(clCfg$hosts, "environment", unlist = F));
	if (Log.level() >= 7) print(clCfg);

	#browser();
	# debugging 6.6.2025, establishEnvironment==T causes massive memory usage
	#	even without invokation of jobInit
	if (establishEnvironment) {
		r = clusterApply(cl, seq_along(hosts), function(i, environments, cfg){
			host = hosts[i];
			env = environments[[host]];
			if (!is.null(env$setwd)) setwd(env$setwd);
			if (!is.null(cfg$source)) for (s in cfg$source) source(s, chdir = TRUE);
			if (!is.null(cfg$libraries))
				#for (package in cfg$libraries) library(package, character.only = TRUE);
				# <N> shiny hack
				for (package in cfg$libraries) do.call('library', list(package, character.only = TRUE));
			# <!> as of 3.4.2013: stop support of exporting global variables to enable CRAN submission
			#if (!is.null(env$globalVars))
			#	for (n in names(env$globalVars)) assign(n, env$globalVars[[n]], pos = .GlobalEnv);
			#sprintf("%s - %s - %s", host, hapmap, getwd());
			if (Nif(cfg$doSinkOutput)) sink(sprintf('%s-%d.out', cfg$doSinkOutput, i));
			#Do.Call(cfg$jobInit);	# accepts NULL objects
			NULL
		}, environments = envs, cfg = clCfg);
		nlapply(clCfg[['varsEnv']], function(envName) {
			envir = get(envName);
			clusterExport(cl, clCfg$varsEnv[[envName]], envir = envir);
		});
	}

	# <p> iterate
	N = clCfg$splitN * length(hosts);	# No of splits
	idcs = splitListIndcs(length(l), N);
	exportNames = c();
	iterator__ = if (clCfg$provideChunkArgument) {
		function(.i, ...) {
			r = lapply(idcs[.i, 1]:idcs[.i, 2], function(j)try(.f(l[[j]], .i, ...)));
			if (class(r) == "try-error") r = NULL;
			r
		}
	} else {
		function(.i, ...){
			r = lapply(idcs[.i, 1]:idcs[.i, 2], function(j)try(.f(l[[j]], ...)));
			if (class(r) == "try-error") r = NULL;
			r
		}
	}
	if (clCfg$evalEnvironment) {
		iterator__ = environment_eval(iterator__, functions = T);
		#clusterExport(cl, varlist = names(as.list(environment(iterator__))), evalEnvironmentenvir = environment(iterator__));
	}
	r = clusterApplyLB(cl, 1:dim(idcs)[1], iterator__, ...);
	# <p> finish up
	if (!clCfg$reuseCluster) stopCluster(cl)
	r = unlist(r, recursive = F);
	#if (clCfg$reverseEvaluationOrder) r = rev(r);
	r
}

# wrapper (as of 3.12.8: I seem to have lost a previous change)
clapply = function(l, .f, ..., clCfg = NULL, .clRunLocal = rget(".clRunLocal", F, envir = .GlobalEnv)) {
	# <p> get cluster specification
	clCfg = merge.lists(
		rget(".globalClusterSpecification", default = list(), envir = Snow_cluster_env__),
		firstDef(clCfg, list())
	);
	# <p> update cluster specification
	clCfg$nestingLevel = clCfg$nestingLevel + 1;
	assign(".globalClusterSpecification", clCfg, envir = Snow_cluster_env__);

	# <p> choose/decline parallelization
	r = if (firstDef(.clRunLocal, clCfg$local, F) || clCfg$nestingLevel != clCfg$splittingLevel) {
		if (clCfg$provideChunkArgument) lapply(X = l, FUN = .f, 1, ...)
		else lapply(X = l, FUN = .f, ...)
	} else {
		clapply_cluster(l, .f, ..., clCfg = clCfg);
	};

	# <p> update cluster specification
	clCfg$nestingLevel = clCfg$nestingLevel - 1;
	assign(".globalClusterSpecification", clCfg, envir = Snow_cluster_env__);
	r
}

#
#	<p> file operations
#

Source_url = function(url, ...) {
	requireNamespace('RCurl');
	request = getURL(url, followlocation = TRUE,
		cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"));
	tf = tempfile();
	writeFile(tf, request);
    source(tf, ...)
}

# <N> environment
if (is.null(rget('GlobalCollections_env__')))
	GlobalCollections_env__ = new.env();

SourceAppendPath = function(path)
	assign('sourceFiles',
		unique(unique(c(unlist(mget('sourceFiles', GlobalCollections_env__, ifnotfound = list(NULL))), path))),
			envir = GlobalCollections_env__);
SourceAppendReset = function()assign('sourceFiles', c(), envir = GlobalCollections_env__);
SourceCollected = function()
	mget('sourceFiles', GlobalCollections_env__, ifnotfound = list(NULL))$sourceFiles;
LibraryAppend = function(lib)
	assign('libraries',
		unique(c(unlist(mget('libraries', GlobalCollections_env__, ifnotfound = list(NULL))), lib)),
			envir = GlobalCollections_env__);
LibraryAppendReset = function()assign('libraries', c(), envir = GlobalCollections_env__);
LibraryCollected = function()
	mget('libraries', GlobalCollections_env__, ifnotfound = list(NULL))$libraries;

SourceSingle = function(file, ..., locations, envir, sourceCollect, logLevel = 6, expand = TRUE) {
	if (expand) file = path.expand(file);
	LogS(logLevel, '[SourceSingle] %{file}q');
	if (sourceCollect && file == '__RESET__')return(SourceAppendReset());
	file0 = file;
	if (isURL(file)) Source_url(file, ...) else {
		file0 = file.locate(file, prefixes = locations);
		if (notE(envir)) sys.source(file = file0, envir = envir, ...) else source(file = file0, ...);
	}
	if (sourceCollect) SourceAppendPath(file0);
}
	
# <!> local = T does not work
Source = function(file, ...,
	locations = c('', '.', sprintf('%s/src/Rscripts', Sys.getenv('HOME'))),
	envir = NULL, sourceCollect = TRUE, logLevel = 6) {
	LogS(logLevel, '[Source] %{file}q');
	sapply(file, SourceSingle, ..., locations = locations, envir = envir, sourceCollect = sourceCollect)
}

firstSystemFile = function(prefix, exts, package, all = FALSE) {
	files = sapply(exts, system.file(package = package) %.% (Sprintf('%{prefix}s.%{.}s')));
	return(if (all) filterListNonEmpty(files) else firstNonEmpty(files));
}

#
#	<p> helper functions readTable/writeTable
#
# -> Rsystem.R

#
#	<p> readTable
#

readTableSplitToDict = function(e){
	r = lapply(splitString(';', e), function(e) {
		r = splitString(':', e);
		listNamed(list(shift(r)), r[1])
	});
	unlist.n(r, 1)
}

# complete: return only complete data with respect to specified colums
# NA: specify 'NA'-values
readTableSepMap = list(T = "\t", S = ' ', C = ',', `;` = ';', `S+` = '');
optionParser2logical = function(e)list(T = T, F = F, `TRUE` = TRUE, `FALSE` = FALSE)[[e]];
optionParser = list(
	SEP = function(e)readTableSepMap[[e]],
	DEC = identity,
	QUOTE = function(e)(if (e == 'F') '' else e),
	HEADER = optionParser2logical,
	ROW.NAMES = function(e)list(T = T, F = F)[[e]],
	NAMES = function(e)splitString(';', e),
	FACTORS = function(e)splitString(';', e),
	NUMERIC = function(e)splitString(';', e),
	AS_CHARACTER = function(e)splitString(';', e),
	DATE = function(e)splitString(';', e),
	# key=value pairs: column name:date format
	DATEF = readTableSplitToDict,
	PROJECT = function(e)splitString(';', e),
	`NA` = function(e)splitString(';', e),
	complete = function(e)splitString(';', e),
	CONST = function(e){ r = lapply(splitString(';', e), function(e){
			r = splitString(':', e);
			v = if (length(fetchRegexpr('^\\d+$', r[2])) > 0) r[2] = as.integer(r[2]) else r[2];
			listKeyValue(r[1], v)
		});
		unlist.n(r, 1)
	},
	HEADERMAP = readTableSplitToDict,
	# tb implemented: <i>: merge.lists recursive
	VALUEMAP = readTableSplitToDict,
	COLNAMESFILE = identity,
	SHEET = as.integer,
	SKIP = as.integer,
	DELETE = Eval,
	ENCODING = identity,
	AS_MIDS = optionParser2logical
);

splitExtendedPath = function(path) {
	q = fetchRegexpr('(?<=^\\[).*?(?=\\]:)', path);
	options = list();
	if (length(q) > 0 && nchar(q) > 0) {
		path = substr(path, nchar(q) + 4, nchar(path));
		os = sapply(splitString(',', q), function(e)splitString('=', e));
		os = listKeyValue(os[1, ], os[2, ]);
		os = nlapply(names(os), function(n)optionParser[[n]](os[[n]]));
		options = merge.lists(options, os);
	}
	r = list(path = path, options = options)
}
path2simple = function(pathes)sapply(pathes, function(path)splitExtendedPath(path)$path);

# https://stackoverflow.com/questions/35121192/reading-a-pickle-file-pandas-python-data-frame-in-r
preparePickle = function() {
	Require('reticulate');
	py_install('pandas');
	py = "import pandas as pd\n\ndef py_read_pickle(file):\n\tpickle_data = pd.read_pickle(file)\n\treturn pickle_data\n";
	tf = tempFileName('pickle', 'py', inRtmp = TRUE)
	writeFile(tf, py);
	source_python(tf);
}

readPickle_old = function(path) {
	if (!notE(rget('py_read_pickle'))) preparePickle();
	o = py_read_pickle(path);
	return(o);
}
readPickle = function(path) {
	library(reticulate);
	pd = import("pandas");
	data = pd$read_pickle(path);
	return(data);
}

readTable.pickle = function(path, options = NULL) {
	d = readPickle(path);
	return(d);
}

readTable.ods = function(path, options = NULL, verbose = F) {
	Library('readODS');
	sheet = firstDef(options$SHEET, 1);
	silence(read_ods(path, sheet = sheet, col_names = options$HEADER),
		verbose = firstDef(options$VERBOSE, verbose)
	);
}

notEC = function(e)(length(e) > 0 && (length(e) > 1 || e[1] != ''))

# <!> changed SEP default "\t" -> ",", 20.5.2015
#readTable.csv.defaults = list(HEADER = T, SEP = "\t", `NA` = c('NA'), QUOTE = '"');
readTable.csv.defaults = list(HEADER = T, SEP = ",", `NA` = c('NA'), QUOTE = '"', SKIP = 0);
readTable.txt = readTable.csv = function(
	path, options = readTable.csv.defaults, headerMap = NULL, setHeader = NULL, ...) {

	options = merge.lists(readTable.csv.defaults, options);
	t = read.table(path, header = options$HEADER, sep = options$SEP, as.is = T,
		na.strings = options$`NA`, comment.char = '', quote = options$QUOTE, skip = options$SKIP, ...);
	if (!is.null(options$NAMES)) names(t)[1:length(options$NAMES)] = options$NAMES;
	if (!is.null(headerMap)) names(t) = vector.replace(names(t), headerMap);
	if (!is.null(setHeader))
		names(t) =  c(setHeader, names(t)[(length(setHeader)+1): length(names(t))]);
	if (notEC(options$FACTORS)) t = Df_(t, as_factor = options$FACTORS);
	if (notEC(options$AS_CHARACTER)) t = Df_(t, as_character = options$AS_CHARACTER);
	t
}

readTable.csv2.defaults = list(HEADER = T, SEP = ";", `NA` = c('NA'), QUOTE = '"', SKIP = 0);
readTable.csv2 = function(
	path, options = readTable.csv2.defaults, headerMap = NULL, setHeader = NULL, ...) {
	options = merge.lists(readTable.csv2.defaults, options);
	return(readTable.csv(path, options, headerMap, setHeader, ...));
}

spssDate = function(date, tz = 'MET', spss.origin = as.POSIXct('2003/02/11', tz = tz) - 13264300800)
	(spss.origin + date)

# convert "labelled" columns to factors
haven2earth = function(d)Df_(lapply(d, function(c) if (is.labelled(c)) as_factor(c) else c))

# ENCODING=latin1 on error 'Unable to convert string to the requested encoding (invalid byte sequence)'
readTable.SAV = readTable.sav = function(path, options = NULL, headerMap = NULL, stringsAsFactors = F) {
	# read file
	#Require('foreign');
	#r0 = suppressWarnings(read.spss(path));
	Library('haven');
	rRaw = if (nif(options) && nif(options$VERBOSE))
		# read_spss -> read_sav (28.2.2023)
		haven::read_sav(path, encoding = options$ENCODING) else
		#read_spss(path) else
		suppressWarnings(haven::read_sav(path, encoding = options$ENCODING));
	r0 = haven2earth(rRaw);
	r1 = as.data.frame(r0, stringsAsFactors = stringsAsFactors);
	# <!> uncommented on 5.10.2021, no prior comment as to why commented out
	# as of 20.10.2021: some dates alredy converted -> check
	dateAlready = sapply(options$DATE, function(e)any(class(r1[[e]]) == 'POSIXct'));
	dates = options$DATE[!dateAlready];
	if (notE(dates)) r1[, dates] = do.call(data.frame, lapply(r1[, dates, drop = F], spssDate));
	r1
}

readTable.dta = function(path, options = NULL, headerMap = NULL, stringsAsFactors = F) {
	Library('haven');
	rRaw = if (nif(options) && nif(options$VERBOSE))
		read_stata(path) else
		suppressWarnings(read_stata(path));
	r1 = as.data.frame(rRaw, stringsAsFactors = stringsAsFactors);
	r1
}


readTable.RData = function(path, options = NULL, headerMap = NULL) {
	t = as.data.frame(get(load(path)[1]), stringsAsFactors = F);
	#print(t);
	t
}

readTable.rds = function(path, options = list(), headerMap = NULL) {
	t = readRDS(path);
	if ('mids' %in% class(t)) {
		t = if (nif(options$AS_MID))
			complete(t, action = 'long', include = TRUE) else
			complete(t, firstDef(options$SHEET, 1));
	}
	#print(t);
	t
}
readTable.rds.post = function(t, options) {
	LogS(4, '[readTable.rds.post] AS_MIDS=%{AS_MIDS}d', options);
	return(if (nif(options$AS_MIDS)) as.mids(t) else t);
}


readTable.xls = function(path, options = NULL, ..., row.names = NULL) {
	sheet = firstDef(options$SHEET, 1);
	Library('gdata', quietly = TRUE);
	read.xls(path, sheet = sheet, ..., row.names = row.names, verbose = FALSE);
}
readTable.xlsx = function(path, options = NULL, ..., row.names = NULL) {
	#Library('xlsx', quietly = TRUE);
	#data = read.xlsx(path, sheetIndex = sheet, ...);
	sheet = firstDef(options$SHEET, 1);
	Library('readxl', quietly = TRUE);
	data = read_xlsx(path, sheet = sheet,
		col_names = firstDef(options$HEADER, TRUE), ...
	);
	return(Df_(data));
}

#tableFunctionConnect = c('csv', 'RData', 'gz');
tableFunctionConnect = c('bz2', 'RData', 'gz');
readersAcceptingFilesOnly = c('xls');

tableFunctionForPathMeta = function(path, template = 'readTable.%{ext}s', default = readTable.csv,
	forceReader = NULL) {
	sp = splitPath(path);
	compression = NULL;
	tmpFile = NULL;
	if (firstDef(forceReader, sp$ext) %in% c('bz2', 'gz')) {
		compression = sp$ext;
		sp = splitPath(sp$fullbase);
		tmpFile = Sprintf('%{file}s.%{ext}s', file = tempfile(), ext = sp$ext);
	}
	name = Sprintf(template, ext = firstDef(forceReader, sp$ext));
	f = if (exists(name)) get(name) else default;
	r = list(
		fct = f, name = name, ext = sp$ext,
		compression = compression, tempfile = tmpFile, path = path
	);
	r
}
tableFunctionForPath = function(path, template = 'readTable.%{ext}s',
	default = readTable.csv, forceReader = NULL) {
	tableFunctionForPathMeta(path, template, default, forceReader)$fct
}

# forceReader: force readerFunction
tableFunctionForPathReader = function(path, template = 'readTable.%{ext}s', default = readTable.csv,
	forceReader = NULL) {
	m = m0 = tableFunctionForPathMeta(path, template = 'readTable.%{ext}s', default = default, forceReader);
	if (!is.null(m$compression)) {
		path = if (m0$compression %in% tableFunctionConnect &&
				 !(m0$ext %in% readersAcceptingFilesOnly))
			compressedConnection(m0$path, m0$compression) else
			decompressPath(m0$path, m0$tempfile, m0$compression)$destination
		m = merge.lists(m0, list(path = path));
	}
	m
}

readTableDate = function(o, colName, col, defaultTz = 'MET') {
	dateFormat = o$DATEF[[colName]];
	r = strptime(col, dateFormat[1], tz = firstDef(dateFormat[2], defaultTz));
	r
}

readTableRecodeNAs = function(tab, NAs) {
	ns = names(tab);
	for (n in ns) if (class(tab[[n]]) == 'character') tab[[n]][ tab[[n]] %in% NAs] = NA;
	return(tab);
}

readTable.defaultOptions = list(HEADER = TRUE, hash = list(do = FALSE, type = 'sha256', hashFile = NULL));
# <!> as of 23.5.2014: headerMap after o$NAMES assignment
# <i> use tableFunctionForPath
#	options:
#		hashFile:	store file hash in propertyList. Create entry if not present, check otherwise
readTableSingle = function(path, autodetect = T, headerMap = NULL, extendedPath = T, colnamesFile = NULL, ...,
	as_factor = NULL, stringsAsFactors = F, defaultReader = readTable.csv, doRemoveTempFile = TRUE,
	forceReader = NULL, ssh = F, options = list()) {
	# <p> preparation
	pathOrig = path = join(path, '');
	#o = merge.lists(readTable.defaultOptions, options);
	o = merge.lists.recursive(readTable.defaultOptions, options);
	if (extendedPath) {
		r = splitExtendedPath(path);
		path = r$path;
		o = merge.lists(o, r$options);
		# print(r$options); print(o);
	}

	sp = splitPath(path, ssh = ssh);
	# <p> copy remote files
	if (sp$host != '') {
		tmpPath = tempFileName('readTable', sp$ext, inRtmp = T);
		File.copy(path, tmpPath);
		path = tmpPath;
		sp = splitPath(path);
	}
	# <!> code not re-entrant, possible race-condition
	if (notE(o$hash) && notE(o$hash$hashFile)) {
		h = if (File.exists(o$hash$hashFile))
			propertyFromString(readFile(o$hash$hashFile)) else list(hashes = list());
		hash = hashPathContent(path, o$hash$type);
		# <A> use only filename to identify path -> add option for full path <i>
		hashS = h$hashes[[ sp$file ]]$hash;
		if (notE(hashS)) {
			if (hashS != hash) {
				LogS(5, 'File: %{file}s hash failure: %{hash}s [cmputed != %{hashS}s [stored]', sp);
				stop('File hash failure on readTable');
			}
			LogS(5, 'Hash check passed: %{file}s: %{hash}s', sp);
		} else {
			h$hashes[[ sp$file ]] = list(hash = hash, type = o$hash$type);
			writeFile(o$hash$hashFile, stringFromProperty(h));
			LogS(5, 'Hash initialized: %{file}s: %{hash}s', sp);
		}
	}
	# <p> read table raw
	reader = if (autodetect && !is.null(sp$ext)) 
		tableFunctionForPathReader(path, 'readTable.%{ext}s', readTable.csv, forceReader) else
		list(fct = defaultReader, path = path);
	r = reader$fct(reader$path, options = o, ...);

	# <p> cleanup
	if (doRemoveTempFile && !get('.system.doLogOnly', envir = System_env__) && !is.null(reader$tempfile))
		file.remove(reader$tempfile);

	# <p> table transformations
	if (!is.null(o$NAMES) && length(o$NAMES) <= ncol(r)) names(r)[1:length(o$NAMES)] = o$NAMES;
	colnamesFile = firstDef(o$COLNAMESFILE, colnamesFile);
	headerMap = c(headerMap, o$HEADERMAP);
	if (!is.null(headerMap)) names(r) = vector.replace(names(r), headerMap);
	if (!is.null(colnamesFile)) {
		ns = read.table(colnamesFile, header = F, as.is = T)[, 1];
		names(r)[1:length(ns)] = ns;
	}
	if (!is.null(o$PROJECT)) r = r[, o$PROJECT, drop = FALSE];
	if (!is.null(o$complete)) r = r[apply(r[, o$complete], 1, function(e)!any(is.na(e))), ];
	if (!is.null(o$CONST)) { for (n in names(o$CONST)) r[[n]] = o$CONST[[n]]; }
	if (notEC(as_factor)) r = Df_(r, as_factor = as_factor);
	if (notEC(o$FACTORS)) r = Df_(r, as_factor = o$FACTORS);
	if (Nif(o$NUMERIC)) r = Df_(r, as_numeric = o$NUMERIC);
	if (!is.null(o$DELETE)) r = r[-o$DELETE, , drop = F];
	if (notE(o[['DATEF']])) { for (n in names(o$DATEF)) r[[n]] = readTableDate(o, n, r[[n]]); }
	if (notE(o[['NA']]) && !(sp$ext %in% c('csv', 'txt', 'csv2'))) r = readTableRecodeNAs(r, o[['NA']]);
	r = callIfExists(Sprintf('readTable.%{ext}s.post', sp), list(r, options = o));
	r
}

# <i> joining operation on multiple provided tables -> interface for join var
readTable = function(path, autodetect = T, headerMap = NULL, extendedPath = T, colnamesFile = NULL, ...,
	as_factor = NULL, stringsAsFactors = F, defaultReader = readTable.csv, doRemoveTempFile = TRUE,
	forceReader = NULL, ssh = F, options = list(), noJoin = FALSE) {

	if (noJoin) {
		lapply(path, readTableSingle,
			autodetect = autodetect, headerMap = headerMap, extendedPath = extendedPath,
			colnamesFile = colnamesFile,
			...,
			as_factor = as_factor, stringsAsFactors = stringsAsFactors,
			defaultReader = defaultReader, doRemoveTempFile = doRemoveTempFile,
			forceReader = forceReader, ssh = ssh, options = options
		)
	} else readTableSingle(path, autodetect, headerMap, extendedPath, colnamesFile, ...,
		as_factor = as_factor, stringsAsFactors = stringsAsFactors,
		defaultReader = defaultReader, doRemoveTempFile = doRemoveTempFile,
		forceReader = forceReader, ssh = ssh, options = options)
}

# o: list with entries: object: object to be saved, name: name of object
saveRDA = function(o, path) {
	Dir.create(splitPath(path)$dir);
	if (is.list(o) && !any(class(o) == 'data.frame'))  {
		assign(o$name, o$object)	# make object known under name nm
		save(o$name, file = path);
	} else {
		nm = splitPath(path)$file;
		assign(nm, o)	# make object known under name nm
		save(nm, file = path);
	}
}

resaveTypes = list(rds = saveRDS, rda = saveRDA);
ResaveTableSingle = function(path, ..., type = 'rds',
	doSave = FALSE, outputFolder = NULL, ignoreExisting = TRUE) {
	output = if (notE(outputFolder))
		Sprintf('%{outputFolder}s/%{base}s.%{type}s', splitPath(path)) else
		Sprintf('%{fullbase}s.%{type}s', splitPath(path));
	if (tolower(splitPath(path)$ext) == type) {
		if (!file.exists(output)) file.copy(path, output);
	} else if (doSave && (!file.exists(output) || !ignoreExisting)) {
		tab = readTable(path, ...);
		resaveTypes[[type]](tab, output);
	}
	return(output)
}

ResaveTable = function(paths, type = 'rds', ..., doSave = FALSE, outputFolder = NULL, ignoreExisting = TRUE)
	lapply(paths, ResaveTableSingle, ..., type = type, doSave = doSave,
		outputFolder = outputFolder, ignoreExisting = ignoreExisting);

resaveTableRdsSingle = function(path, ..., doSave = FALSE) {
	if (tolower(splitPath(path)$ext) == 'rds') return(path);
	output = Sprintf('%{fullbase}s.rds', splitPath(path));
	if (doSave) {
		tab = readTable(path, ...);
		saveRDS(tab, output);
	}
	return(output)
}
resaveTableRds = function(paths, ..., doSave = FALSE)
	lapply(paths, resaveTableRdsSingle, ..., doSave = doSave);

resaveTableSingle = function(path, ..., outputDir = NULL, extension = 'rds', doSave = FALSE, createDir = TRUE, baseExtend = '', force = FALSE) {
	sp = splitPath(path);

	if (tolower(sp$ext) == extension) return(path);
	#outputDir = ifelse(notE(outputDir), outputDir, sp$dir)
	#output = Sprintf('%{outputDir}s/%{base}s.%{extension}s', sp);
	output = pathReplace(path, extension, outputDir, baseExtend);
	LogS(5, 'Resaving "%{path}s" -> "%{output}s"');
	if (doSave) {
		tab = readTable(path, ...);
		# <i> modification date check
		if (!file.exists(output) || force)
			writeTable(tab, output, createDir = createDir) else
			LogS(5, '[resaveTableSingle] Not resaved. "%{output}s" exists. ');
	}
	return(output)
}

resaveTable = function(paths, ..., outputDir = NULL, extension = 'rds', doSave = FALSE, createDir = TRUE, baseExtend = '', force = FALSE, doSimplify = TRUE) {
	paths = lapply(paths, resaveTableSingle, ...,
		outputDir = outputDir, extension = extension,
		doSave = doSave, createDir = createDir,
		baseExtend = baseExtend, force = force);
	return(if (doSimplify) simplify(paths) else paths);
}

ReadTable = function(path, ...,
	resaveLocation = NULL, resaveAs = 'rds', doResave = TRUE) {
	pathRead = if (doResave) {
		resaveTable(path, ..., outputDir = resaveLocation, extension = resaveAs, createDir = TRUE, baseExtend = '_resavescache', force = FALSE, doSave = TRUE);
	} else path;
	LogS(5, '[ReadTable] Reading from path %{pathRead}s');
	return(readTable(pathRead, ...));
}

#
#	<p> writeTable
#

writeTable.defaults = list(
	global = list(
	SEP = ' ', DEC = '.',
	ROW.NAMES = FALSE,
	HEADER = TRUE,
	QUOTE = TRUE
	),
	csv = list(SEP = ',', DEC = '.'),
	csv2 = list(SEP = ';', DEC = ',')
);
writeTable.table = function(dataFrame, path, ..., doCompress = NULL, row.names = TRUE, options = list()) {
	o = merge.lists(writeTable.defaults$global, list(ROW.NAMES = row.names), options);
	conn = compressedConnection(path, doCompress, mode = 'w');
	with(o, write.table(dataFrame, file = conn, ...,
		row.names = ROW.NAMES, col.names = HEADER, sep = SEP, quote = (QUOTE != '')));
}

writeTable.compression = function(object, path, doCompress = NULL, row.names = TRUE,
	doRemoveOrig = TRUE, options = list(), writerFunction) {

	pathRaw = if (!is.null(doCompress)) Sprintf('%{path}s_raw_') else path;
	writerFunction(object, pathRaw, row.names = row.names, options = options);
	r1 = compressPath(pathRaw, path, doCompress, doRemoveOrig);
	r1
}

writeTable.sav_raw = function(object, path, doCompress = NULL, row.names = TRUE, doRemoveOrig = TRUE,
	options = list()) {
	# <p> required package
	options( java.parameters = "-Xmx4g" );
	Require('haven');

	# sanitize row names
	d0 = as.data.frame(object);
	names(d0) = gsub("[.\\$@# -]", "\\_", names(d0));
	# <A> col names in SPSS are case insensitive
	names(d0) = disambiguate(names(d0), sep = '_');
	write_sav(d0, path);
}

writeTable.sav = function(object, path, doCompress = NULL, row.names = TRUE,
	doRemoveOrig = TRUE, options = list()) {
	writeTable.compression(object, path, writerFunction = writeTable.sav_raw,
		doCompress = doCompress, row.names = row.names, doRemoveOrig = doRemoveOrig, options = options);
}

writeTable.rds = function(object, path, doCompress = NULL, row.names = TRUE,
	doRemoveOrig = TRUE, options = list()) {
	saveRDS(object, path);
}

writeTable.xlsx_old = function(object, path, doCompress = NULL, row.names = TRUE,
	doRemoveOrig = TRUE, options = list()) {
	Library('writexl');
	options( java.parameters = "-Xmx4g" );
	Require('xlsx');
	r = path;
	# <p> save non-data frame-constrained column names
	colNames = if (is.matrix(object)) dimnames(object)[[2]] else NULL;
	dataFrame = as.data.frame(object);
	r0 = pathRaw = if (!is.null(doCompress)) Sprintf('%{path}s_raw_') else path;

	# <p> construct spreadsheet
	wb = createWorkbook();
	cs = CellStyle(wb, dataFormat = DataFormat("#,##0.00"));
	styles = listKeyValue(1:ncol(dataFrame), rep.list(cs, ncol(dataFrame)));
    sheet = createSheet(wb, 'dataFrame')
    addDataFrame(dataFrame, sheet, col.names = TRUE, row.names = row.names, 
        startRow = 1, startColumn = 1, colStyle = styles, colnamesStyle = NULL, 
        rownamesStyle = NULL);
    saveWorkbook(wb, pathRaw);

    #r0 = write.xlsx2(dataFrame, file = pathRaw, row.names = row.names, showNA = FALSE,
	#	colStyle = styles);
	r1 = compressPath(pathRaw, path, doCompress, doRemoveOrig);
	r0
}
writeTable.xlsx = function(object, path, doCompress = NULL, row.names = TRUE,
	doRemoveOrig = TRUE, options = list()) {
	Library('writexl');
	pathRaw = if (!is.null(doCompress)) Sprintf('%{path}s_raw_') else path;
	write_xlsx(object, pathRaw,
		col_names = TRUE, format_headers = TRUE, use_zip64 = FALSE);
	r1 = compressPath(pathRaw, path, doCompress, doRemoveOrig);
	pathRaw
}

writeTable.xls = function(object, path, doCompress = NULL, row.names = TRUE,
	doRemoveOrig = TRUE, options = list()) {
	Library('WriteXLS');
	r = path;
	dataFrame = as.data.frame(object);
	pathRaw = if (!is.null(doCompress)) Sprintf('%{path}s_raw_') else path;
	r0 = WriteXLS(dataFrame, ExcelFileName = pathRaw, row.names = row.names);
	r1 = compressPath(pathRaw, path, doCompress, doRemoveOrig);
	r0
}

writeTable.csv2 = writeTable.csv = function(dataFrame, path, ..., doCompress = NULL,
	row.names = TRUE, options = list()) {
	conn = compressedConnection(path, doCompress, mode = 'w');
	with(options, LogS(5, 'Write table: SEP="%{SEP}s", HEADER=%{HEADER}s, quote=%{QUOTE}s'));
	with(options, write.table(dataFrame, file = conn, ...,
		sep = SEP, col.names = HEADER, row.names = ROW.NAMES, quote = (QUOTE != ''), dec = DEC));
}
# doCompress = 'bz2' to write bz2
# <i><!> determine from path
writeTableRaw = function(object, path, ..., doCompress = NULL, row.names = TRUE, autodetect = TRUE,
	defaultWriter = writeTable.csv, options = list(), createDir = FALSE) {
	LogS(5, '[writeTableRaw] Writing table: %{path}s');
	sp = splitPath(path);
	if (createDir) Dir.create(sp$dir, recursive = TRUE);
	if (is.null(doCompress) && !is.null(sp$ext) && sp$ext %in% c('bz2', 'gz')) {
		doCompress = sp$ext;
	}
	writer = if (autodetect && !is.null(sp$ext)) 
		tableFunctionForPath(path, 'writeTable.%{ext}s', writeTable.csv) else defaultWriter;
	if (is.null(writer))
		stop(Sprintf("[writeTableRaw] Writing table to extension '%{ext}s' not supported [path=%{path}s]", ext = sp$ext));
	options = merge.lists(writeTable.defaults$global,
		writeTable.defaults[[firstDef(sp$ext, 'global')]], options);
	r0 = writer(object, path = path, ..., doCompress = doCompress, row.names = row.names, options = options);
	r = list(path = path, return = r0);
	r
}

writeTable = function(object, path, ..., doCompress = NULL, row.names = TRUE, autodetect = TRUE,
	defaultWriter = writeTable.csv, simplify = TRUE, extendedPath = TRUE) {
	o = list();
	if (extendedPath) {
		r = splitExtendedPath(path);
		path = r$path;
		o = r$options;
		defaultWriter = writeTable.table;
	}
	if (!is.null(o$PROJECT)) object = object[, o$PROJECT, drop = FALSE];
	r = lapply(path, function(p)
		writeTableRaw(object, p, ...,
			doCompress = doCompress, row.names = row.names, autodetect = autodetect,
			defaultWriter = defaultWriter, options = o)
	);
	if (simplify && length(path) == 1) r = r[[1]];
	r
}

#
#	<p> swig
#

swigIt = function(interface, code, moduleName = NULL) {
	dir = tempdir();	# will be constant across calls
	if (is.null(moduleName)) {
		t = tempFileName("swig");
		moduleName = splitPath(t)$base;
	}

	ifile = sprintf("%s/%s.%s", dir, moduleName, "i");
	interface = sprintf("
		%%module %s
		%%inline %%{
			%s;
		%%}
	", moduleName, paste(interface, collapse = ";\n\t\t\t"));

	ifile = sprintf("%s/%s.%s", dir, moduleName, "i");
	base = splitPath(ifile)$fullbase;
	writeFile(ifile, interface);
	cfile = sprintf("%s.c", base);
	writeFile(cfile, code);
	#print(list(i = ifile, c = cfile, so = sprintf("%s.so", base)));
	system(sprintf("swig -r %s", ifile));
	#cat(code);

	system(sprintf("cd %s ; gcc -O2 -D__USE_BSD -D__USE_GNU -std=c99 -c -fpic %s.c %s_wrap.c -I/usr/local/lib64/R/include -lm ",
		splitPath(ifile)$dir, base, base));
	system(sprintf("cd %s ; gcc -shared %s.o %s_wrap.o -o %s.so", splitPath(ifile)$dir, base, base, base));
	#dyn.unload(sprintf("%s.so", base));
	dyn.load(sprintf("%s.so", base));
	source(sprintf("%s/%s.R", splitPath(ifile)$dir, moduleName));
}

#
#	<p> print
#

fprint = function(..., file = NULL, append = F) {
	if (!is.null(file)) sink(file = file, append = append);
	r = print(...);
	if (!is.null(file)) sink();
	r
}


#
#	crypotgraphy/checksumming
#

#
#	<p> package documentation
#

#	docFile = sprintf('%s/tmp/docOut.Rd', Sys.getenv('HOME'));
#	docDir = sprintf('%s/src/Rpackages/parallelize.dynamic/parallelize.dynamic/man', Sys.getenv('HOME'));
#	docs = RdocumentationSkeleton('Rparallel.back.R', 'parallelize.dynamic', output = docFile);
#	writeRdocumentationToDir(docFile, docDir);

RdocumentationForObjects = function(items, envir, unparser = function(item, envir)item) {
	files = suppressMessages({
		sapply(items, function(item)unparser(item, envir));
	});
	docs = lapply(files, readFile);
	names(docs) = sapply(files, function(f)splitPath(f)$base);
	docs
}
RdocumentationForFunctions = function(items, envir) {
	docs = RdocumentationForObjects(items, envir, unparser = function(item, envir) {
		file = file.path(tempdir(), sprintf("%s.Rd", item));
		prompt(get(item, envir = envir), name = item, filename = file);
		file
	});
	docs
}
RdocumentationForClasses = function(items, envir) {
	docs = RdocumentationForObjects(items, envir, unparser = function(item, envir) {
		file = file.path(tempdir(), sprintf("%s-class.Rd", item));
		methods::promptClass(item, filename = file, where = envir);
		file
	});
	docs
}
RdocumentationForMethods = function(items, envir) {
	docs = RdocumentationForObjects(items, envir, unparser = function(item, envir) {
		file = file.path(tempdir(), sprintf("%s-methods.Rd", item));
		methods::promptMethods(item, filename = file, findMethods(item, where = envir));
		file
	});
	docs
}


# code from packages.skeleton
objectsFromCodeFiles = function(R_files, packageName = 'generic') {
	e = new.env(hash = T);
	methods::setPackageName(packageName, e);
	for (f in R_files) sys.source(f, envir = e);
	classes = getClasses(e);
	methods = getGenerics(e);
	others = ls(e, all.names = T);
	others = others[grep('^\\.', others, invert = T)];

	r = list(envir = e, classes = classes, methods = methods,
		others = setdiff(setdiff(others, classes), methods));
	r
}

RdocumentationSkeleton = function(R_files, output = NULL, packageName = 'generic') {
	os = objectsFromCodeFiles(R_files, packageName = packageName);
	docs = c(
		RdocumentationForFunctions(os$others, os$envir),
		RdocumentationForClasses(os$classes, os$envir),
		RdocumentationForMethods(os$methods, os$envir)
	);

	doc = join(nlapply(docs, function(n) {
		sprintf("\nDOCUMENTATION_BEGIN:%s\n%s\nDOCUMENTATION_END\n", n, docs[[n]])
	}), "\n");
	if (!is.null(output)) {
		if (File.exists(output)) {
			Log(sprintf("Move away file '%s' before writing new skeleton", output), 2);
		} else {
			writeFile(output, doc);
		}
	}
	doc
}

writeRdocumentationToDir = function(pathesIn, pathOut, cleanOut = F) {
	doc = sapply(pathesIn, readFile, USE.NAMES = F);
	r = unlist.n(getPatternFromStrings(doc, '(?s)(?:\\nDOCUMENTATION_BEGIN:)([^\\n]+)\\n(.*?)(?:\\nDOCUMENTATION_END\\n)'), 1);
	Dir.create(pathOut, recursive = T);
	if (cleanOut) {
		files = list_files_with_exts(pathOut, 'Rd');
		file.remove(files);
	}
	nlapply(r, function(n) {
		output = file.path(pathOut, sprintf('%s.Rd', n));
		Log(sprintf('Writing to %s', output), 3);
		writeFile(output, r[[n]]);
	});
	names(r)
}

reDoc = function(package = 'parallelize.dynamic',
	docFile = sprintf('./%s.doc.Rd', package), docDir = sprintf('./%s/man', package)) {
	writeRdocumentationToDir(docFile, docDir, cleanOut = T);
	install.packages(sprintf('./%s', package), repos = NULL);
	#detach(package);
	#library(package)
}

#
#	<p> Rcpp helpers
#

# mod = createModule('module_parametrizer', 'build/libhaplotypes_rcpp.so',
#	headers = c('parameterhelper.h', 'parametrizer.h', 'parametrizerrcpp.h', 'parametrizerrcpp_module.h'),
#	output = 'mod'
#);

#mod = moduleCreate('Rmmap.h', 'build/libmmap_rcpp.so', output = 'mod_mmap');

# interface: C++ header file w/ RCPP_MODULE declaration
#	code block between '// -- begin inline Rcpp' and '// -- end inline Rcpp' is used
moduleCreate = function(interface, libpathes = c(), headers = c(), output = NULL, cppVersion = '13', cppFlags = '') {
	Require('Rcpp');
	Require('inline');

	# <p> libs tb linked against
	#sapply(libpathes, function(e)splitPath(e)$dir);
	dirs = list.kpu(splitPath(libpathes, simplify = F), 'dir');
	libs = sapply(libpathes, \(e)fetchRegexpr('(?<=lib)(.*)(?=.so)', splitPath(e)$file));
	.libPaths(c(.libPaths(), dirs));
	libincludes = join(sapply(seq_along(dirs), \(i)sprintf('-L"%s" -l%s', splitPath(dirs[i])$absolute, libs[i])), ' ');

	# <p> prepare environment
	Sys.setenv(`PKG_LIBS` = sprintf('%s %s', Sys.getenv('PKG_LIBS'), libincludes));
	# <!> hardcoded include path as of 2/2021
	flags = Sprintf('%{libs}s %{rcppFlags}s -I/usr/include/c++/%{cppVersion}s -I%{cwd}q %{cppFlags}s',
		libs = Sys.getenv('PKG_LIBS'),
		rcppFlags = stdOutFromCall(Rcpp:::CxxFlags()),
		cwd = getwd());
	Sys.setenv(`PKG_CXXFLAGS` = flags);
	for (lib in libpathes) { dyn.load(lib, local = F) }

	# <p> create definition file
	moduleRegex = '(?s:(?<=// -- begin inline Rcpp\n)(.*?)(?=// -- end inline Rcpp))';
	inc = join(sapply(interface, \(f)fetchRegexpr(moduleRegex, readFile(f))), "\n");
	inc = join(c(sapply(headers, \(f)Sprintf('#include "%{f}s"\n')), inc), "\n");
	print(inc);
	moduleName = FetchRegexpr('RCPP_MODULE[(](.*?)[)]', inc, captures = T);
	LogS(2, 'Creating module %{moduleName}s');

	# <p> create module
	#rcpp = cxxfunction( signature(), '' , includes = inc, plugin = 'Rcpp', verbose = T );
	rcpp = cxxfunction( , '' , includes = inc, plugin = 'Rcpp', verbose = T );
	mod = Module( moduleName,  getDynLib(rcpp) );
	if (notE(output)) {
		Dir.create(output, recursive = TRUE);
		libfiles = sapply(libpathes, \(lib) {
			File.copy(lib, Sprintf('%{output}s/%{file}s', splitPath(lib)), symbolicLinkIfLocal = F);
			splitPath(lib)$file
		});
		# use name of first module
		glue = Sprintf('%{output}s/%{m}s.so', m = moduleName[1]);
		File.copy(getDynLib(rcpp)[['path']], glue, symbolicLinkIfLocal = F);
		module_descriptor = list(
			name = moduleName,
			libs = c(libfiles, splitPath(glue)$file)
		);
		saveRDS(module_descriptor, file = Sprintf('%{output}s/module.rds'));
	}
	mod
}

moduleActivate = function(path) {
	Require('Rcpp');
	module_descriptor = readRDS(Sprintf('%{path}s/module.rds'));
	libPaths = sapply(module_descriptor$libs, \(.)Sprintf('%{path}s/%{.}s'));
	r = lapply(libPaths, \(.)try(dyn.unload(.), silent = T));
	r = lapply(libPaths, \(.)dyn.load(., local = F));
	# print(r);
	mod = Module( module_descriptor$name, rev(r)[[1]] );
	#mod = Module( module_descriptor$name, r[[1]] );
	mod
}


#
#	<p> sqlite
#
sqlCreateTable = function(columns, types = list, index = NULL, createAt = NULL, unique = list(),
	tableName = 'data') {
	# <p> types
	types = merge.lists(listKeyValue(columns, rep('text', length(columns))), types);
	# <p> cols
	cols = join(sep = ', ', sapply(columns, function(e)sprintf('%s %s', qs(e, force = T), types[e])));
	# <p> unique constraints
	#UNIQUE (col_name1, col_name2) ON CONFLICT REPLACE)
	unique = circumfix(join(sapply(unique, function(u) {
		Sprintf('UNIQUE (%{cols}s) ON CONFLICT FAIL', cols = join(qs(u, force = T), ', '))
	}), ', '), pre = ' , ');
	createDbSql = join(sep = "\n", c(
		Sprintf('CREATE TABLE %{tableName}s (%{cols}s%{unique}s);'),
		if (is.null(index)) c() else sapply(1:length(index), function(i)
			Sprintf('CREATE INDEX index_%{tableName}s_%{i}d ON %{tableName}s (%{cols}s);',
				cols = join(qs(index[[i]], force = T), sep = ', '))),
		'.quit', ''
	));
	if (!is.null(createAt)) {
		Dir.create(createAt, treatPathAsFile = T);
		System(sprintf('echo \'%s\' | sqlite3 %s', createDbSql, qs(createAt)), 5);
	}
	createDbSql
}

# Create sqlite database with contents of csv-file
# @par index: list of columns to index
# @par type: sqlite types: integer, real, text, blob, not specified assumes text
# @par rawFilter pipable command call to filter rows

sqlTypeMap = list(integer = 'integer', numeric = 'real', character = 'text');
csv2sqlitSepMap = readTableSepMap;
sepMap = list(T = '\\t', S = ' ', C = ',', `;` = ';', `S+` = ' ');
sepMapCut = list(T = '\\t', S = '" "', C = ',', `;` = ';', `S+` = '" "');
csv2sqlite = function(path, output = tempfile(),
	columnsNames = NULL, columnsSelect = NULL,
	index = NULL,
	inputSep = 'T', inputHeader = T, inputSkip = 0,
	NULLs = NULL, types = list(), newDb = T, unique = list(),
	tableName = 'data', removeLeadingWS = T, headerMap = NULL,
	rawFilter = NULL) {
	if (is.null(output)) stop(Sprintf('No output path given for sqlite DB [NULL]@csv2sqlite'));
	if (newDb && file.exists(output)) file.remove(output);
	# <!> cave: does not heed skip
	if (inputHeader && is.null(columnsNames)) {
		columnsNames = read.table(path, header = F, nrows = 1, sep = csv2sqlitSepMap[[inputSep]]);
	}
	# <p> select columns
	#	<!> cave cut sorts provided indeces
	prefix = if (removeLeadingWS) "sed 's/^\\s\\s*//' |" else '';
	cut = if (notE(columnsSelect)) {
		Isel = which.indeces(columnsSelect, avu(columnsNames));
		columnsSelect = columnsSelect[order(Isel)];	# align ordering
		# <i> S+
		#  cat | sed 's/\s\s*/ /g' | cut -d' ' -f5
		cut = if (inputSep == 'S+')
			Sprintf("%{prefix}s sed 's/\\s\\s*/ /g' | cut") else Sprintf('%{prefix}s cut');
		Sprintf('| %{cut}s %{sep}s -f %{columns}s ',
			sep = if (inputSep == 'T') '' else sprintf('-d %s', sepMapCut[[inputSep]]),
			columns = join(sort(Isel), ',')
		)
	} else '';
	columns = if (notE(columnsSelect)) columnsSelect else columnsNames;
	columnsDb = if (notE(headerMap)) avu(headerMap[columns]) else columns;
	types = merge.lists(listKeyValue(columnsDb, rep('text', length(columns))), types);
	sqlCreateTable(columnsDb, types, index, createAt = output, unique = unique, tableName = tableName);

	# <p> import data
	#skipCommand = if (is.null(inputSkip)) '' else sprintf('| tail -n +%d ', inputSkip + 1);
	# <N> always skip header as column re-mapping might have occured
	skipCommand = sprintf('| tail -n +%d ', inputSkip + 2);
	sp = splitPath(path);
	reader = Sprintf(if (notE(sp$ext) && sp$ext == 'gz') 'zcat %{path}Q ' else 'cat %{path}Q ');
	if (notE(rawFilter)) reader = Sprintf('%{reader}s | %{rawFilter}s ');
	sqlStatements = c(
		#Sprintf(".separator %{sep}Q", sep = sepMap[[inputSep]]),
		Sprintf(".separator \"%{sep}s\"", sep = sepMap[[inputSep]]),
		Sprintf(".import \"/dev/stdin\" %{tableName}s")
	);
	importSqlite = join(sep = "\n", sqlStatements);
	LogS(5, 'Sqlite import statements: %{importSqlite}s');
	importSql = writeFile(tempfile(), importSqlite);

	sepText = sepMap[[inputSep]];
	filter = if (is.null(NULLs)) '' else
		sprintf("| perl -pe 's/((?<=%s)(?:%s)(?=%s|$)|(?<=^)(?:%s)(?=%s|$))//g'",
			sepText, join(NULLs, '|'), sepText, sepText, sepText);
	cmd = Sprintf(con(
		"%{reader}s %{skipCommand}s %{cut}s %{filter}s",
		" | sqlite3 -init %{importSql}Q %{output}Q"));
	System(cmd, 5);
	output
}
# <!> unfinished, siphones code from old csv2sqlite function
url2sqlite = function(url, output = tempfile(), header = NULL, skip = NULL, selectColumns = NULL,
	index = NULL, sep = 'T',
	NULLs = NULL, types = list()) {
	
	# <p> determine header
 	tmp1 = tempfile();
 	ret = download.file(url, tmp1, method, quiet = FALSE, mode = "w", cacheOK = TRUE);
	#if (ret) stop(sprintf("Download of '%s' failed.", url));
	if (is.null(header)) {
		tmpHeader = tempfile();
	}
}

# <!> 7.1.2015: was qq, but conflicts with QQ-plot function
qquote = function(s)as.character(fetchRegexpr('([^ ]+)', s, captures = T))

sqlite2sqlite = function(dbS, dbD, query, cols, types = list(), index = NULL) {
	sqlCreateTable(cols, types, index, createAt = dbD);
	cmd = sprintf("echo %s | sqlite3 -init %s %s | sqlite3 -init %s %s",
		qs(query),
		qs(writeFile(tempfile(), ".mode csv")),
		qs(dbS),
		qs(writeFile(tempfile(), ".separator ,\n.import \"/dev/stdin\" data")),
		qs(dbD)
	);
	System(cmd, 5);
	dbD
}

sqliteOpen = function(path) {
	Require('RSQLite');
	dbConnect(SQLite(), dbname = path);
}

sqliteQueryParticle = function(q) {
	Nq = length(q);
	opRaw = if(Nq > 2) q[[2]] else if (q[[1]] == 'Statement') q[[1]] else 'equal';
	op = switch(opRaw,
		equal = '=',
		`<` = '<',
		`>` = '>',
		`< Int` = '<',
		`> Int` = '>',
		`< Num` = '<',
		`> Num` = '>',
		`< Char` = '<',
		`> Char` = '>',
		'like' = 'LIKE',
		Statement = NA
	);
	suff = if (opRaw %in% c('< Int', '> Int', '< Num', '> Num')) ' + 0' else '';
	value = if (Nq > 2) q[[3]] else q[[2]];
	if (!(opRaw %in% c('< Int', '> Int', '< Num', '> Num', 'Statement'))) value = Sprintf('%{value}Q');
	if (opRaw %in% c('Statement'))
		with(q, Sprintf('%{value}s')) else
		with(q, Sprintf('%{name}Q%{suff}s %{op}s %{value}s'))
}
sqliteBuildQuery = function(table, query = NULL, distinct = TRUE) {
	if (is.null(query)) return(Sprintf('SELECT * FROM %{table}Q'));
	ns = names(query);
	qs = ilapply(query, function(q, i)sqliteQueryParticle(c(list(name = ns[i]), q)));
	condition = join(unlist(qs), sep = ' AND ');
	isDistinct = if (distinct) 'DISTINCT' else '';
	query = Sprintf('SELECT %{isDistinct}s * FROM %{table}Q WHERE %{condition}s');
	query
}

# query: list(chr = info$chr, mapPhy = list('> Int', pos - range), mapPhy = list('< Int', pos + range));
#	list(id = list(like = 'rs%'))

sqliteQuery = function(db, query = list(Statement = '1'), table = NULL, N = -1) {
	if (is.null(table)) table = dbListTables(db)[1];
# 	query = con(sapply(names(query), function(n)Sprintf('%{n}Q = %{v}s', v = qs(query[[n]], force = T))));
# 	query1 = Sprintf('SELECT * FROM %{table}Q WHERE %{query}s');
	query1 = sqliteBuildQuery(table, query);
	Log(query1, 5);
	dbGetQuery(db, query1, n = N);
}

#
#	<p> publishing
#

# if (1) {
#	.fn.set(prefix = 'results/201404/expressionMonocytes-')
# 	initPublishing('expressionMonocytes201404', '201405');
# 	publishFile('results/expressionMonocytesReportGO.pdf');
# }
# # force as subdir in the reporting dir
# publishDir('results/BAP1IHCgekoppeldaanWelofGeenMonosomie3', asSubdir = T);

Publishing_env__ <- new.env();
initPublishing = function(project, currentIteration, publicationPath = '/home/Library/ProjectPublishing',
	logLevel = 5) {
	md5 = md5sumString(project);
	assign('project', project, Publishing_env__);
	assign('projectMd5', md5, Publishing_env__);
	assign('currentIteration', currentIteration, Publishing_env__);
	assign('publicationPath', publicationPath, Publishing_env__);
	LogS(logLevel, "Project tag: %{project}s Md5: %{md5}s");
}
publishFctEnv = function(path, into = NULL, as = NULL) with(as.list(Publishing_env__), {
	if (!exists('project')) stop('Publishing system not yet initialized.');

	projectFolder = Sprintf('%{publicationPath}s/%{projectMd5}s');
	prefix = if (is.null(into)) '' else Sprintf('%{into}s/');
	destinationPrefix = Sprintf('%{projectFolder}s/%{currentIteration}s/%{prefix}s');
	destination = Sprintf('%{destinationPrefix}s%{path}s',
		path = if (is.null(as)) splitPath(path)$file else as);
	r = list(projectFolder = projectFolder, prefix = prefix, destination = destination,
		destinationPrefix = destinationPrefix);
	r
})


publishFileRaw = function(file, into = NULL, as = NULL) with(publishFctEnv(file, into, as), {
	if (!is.null(into)) Dir.create(destination, treatPathAsFile = T, recursive = T);
	Logs('Publishing %{file} --> "%{destination}s', 3);
	Dir.create(splitPath(destination)$dir, recursive = T);
	System(Sprintf("chmod -R a+rX %{dir}s", dir = qs(projectFolder)), 4);
	r = file.copy(file, destination, overwrite = T);
	if (any(!r)) Logs("Copying of '%{file}s' failed.", 3);
	Sys.chmod(destination, mode = '0755', use_umask = F);
	destination
})

publishUnit = function(u, into = NULL, as = NULL, asSubdir = TRUE) {
	if (!file.exists(u)) {
		LogS(4, 'publishUnit: "%{u}s" does not exist');
		return(NULL);
	}
	if (file.info(u)$isdir)
		publishDir(u, into, as, asSubdir) else
		publishFileRaw(u, into, as)
}

publishFiles = publishFile = function(files, into = NULL, as = NULL) {
	lapply(files, publishUnit, into = into, as = as)
}

publishCsv = function(table, as, ..., into = NULL) {
	file = tempfile('publish', fileext = 'csv');
	write.csv(table, file = file, ...);
	publishFile(file, into, as);
}

publishDir = function(dir, into = NULL, as = NULL, asSubdir = FALSE) with(publishFctEnv('', into, as), {
	sp = splitPath(dir);
	# if 'dir' is a slashed dir itself, use the last dir-component
	if (asSubdir) into = if (sp$file == '') splitPath(sp$dir)$file else sp$file;
	if (!is.null(into)) {
		destination = splitPath(Sprintf('%{destination}s/%{into}s/'))$fullbase;	# remove trailing slash
	}
	Dir.create(destination);
	Logs('Publishing %{dir} --> %{destination}s', 3);
	Dir.create(destination, recursive = T);
	System(Sprintf("chmod -R a+rX %{projectFolder}Q"), 4);
	System(Sprintf("cp -r %{dir}Q/* %{destination}Q"), 4);
	System(Sprintf("chmod -R a+rX %{projectFolder}Q"), 4);
	destination
})

publishAsZip = function(files, as, into = NULL, recursive = FALSE) {
	tmp = tempFileName('publishAsZip', createDir = T, inRtmp = T);
	output = tempFileName('publishAsZip', 'zip', inRtmp = T, doNotTouch = T);
	sapply(files, function(file) {
		File.symlink(splitPath(file)$absolute, Sprintf("%{tmp}s"), replace = F);
		NULL
	});
	recursiveOption = ifelse(recursive, '-r', '');
	System(Sprintf("zip -j %{recursiveOption}s %{output}s %{tmp}s/*"), 2);
	publishFile(output, into = into, as = as);
}

#
#	<p> quick pdf generation
#

print2pdf = function(elements, file) {
	es = elements;
	tf = tempfile();
	sink(tf);
		nlapply(es, function(n) {
			cat(n);
			cat('\n-------------------------------------------\n');
			print(es[[n]]);
			cat('\n\n');
		})
	sink();
	System(Sprintf('a2ps %{tf}s --columns 1 --portrait --o - | ps2pdf - - > %{output}s', output = qs(file)));
}

#
#	<p> workarounds
#

# misc

#
#	<p> packages
#

LibraryRaw = function(name, ..., repos, repoNoInteraction, repoDefault, quietly,
	dependencies, reposSupplementary) {
	# force evaluation
	#if (!Eval(Sprintf('require(%{name}s)'))) {
	if (!Require(name, character.only = TRUE, quietly = TRUE)) {
		if (repoNoInteraction) repos[repos == '@CRAN@'] = repoDefault[1];
		repos = c(repos, reposSupplementary);
		#expr = Sprintf('install.packages(%{name}s)');
		r = try(install.packages(name, repos = repos, ..., dependencies = dependencies));
		# if installation from CRAN fails, try bioconductor
		Log(Sprintf('Trying to install "%{name}s" from bioconductor'), 5);
		if (is.null(r) || class(r) == 'try-error') {
			#if (!exists('biocLite')) source("https://bioconductor.org/biocLite.R");
			#biocLite(name, suppressUpdates = TRUE, suppressAutoUpdate = TRUE, dependencies = dependencies)
			# > 3.5.0
			Require('BiocManager');
			BiocManager::install(name, update = FALSE, dependencies = dependencies);
		}
		#Eval(Sprintf('library(%{name}s)'));
		#library(name, character.only = TRUE, quietly = quietly);
		# <N> hack to get shiny running
		do.call('library', list(name, character.only = TRUE, quietly = quietly));
	}
}

# name has to be character of length 1
LibrarySingle = function(name, ...,
	repos = getOption('repos'), repoNoInteraction = TRUE, repoDefault = "http://cran.rstudio.com",
	quietly = TRUE, dependencies = TRUE, reposSupplementary = 'http://www.rforge.net/', LibraryCollect = TRUE) {

	if (LibraryCollect && name == '__RESET__')return(LibraryAppendReset());
	if (LibraryCollect) LibraryAppend(name);
	wrapper = if (quietly) suppressWarnings else eval;
	for (name in name) {
		wrapper(LibraryRaw(name, ...,
			repos = repos, repoNoInteraction = repoNoInteraction, repoDefault = repoDefault,
			quietly = quietly, dependencies = dependencies, reposSupplementary = reposSupplementary));
	}
}
Library = function(name, ...,
	repos = getOption('repos'), repoNoInteraction = TRUE, repoDefault = "http://cran.rstudio.com",
	quietly = TRUE, dependencies = TRUE, reposSupplementary = 'http://www.rforge.net/', LibraryCollect = TRUE) {

	for (name in name) {
		LibrarySingle(name, ..., repos = repos, repoNoInteraction = repoNoInteraction,
			repoDefault = repoDefault, quietly = quietly,
			dependencies = dependencies, reposSupplementary = reposSupplementary,
			LibraryCollect = LibraryCollect
		);
	}
}
Require = function(..., quietly = TRUE) {
	wrapper = if (quietly) function(call_)suppressWarnings(suppressPackageStartupMessages(call_)) else eval
	#wrapper(require(..., quietly = quietly))
	# hack to get shiny running
	wrapper(do.call('require', c(list(...), list(quietly = quietly))))
}

DumpPackageNames = function(path = '~/Documents/AdminComputer/R/packages-%{version}s.csv',
	version = with(R.Version(), Sprintf('%{major}s.%{minor}s')), n = 1) {
	Logs('Writing package names to file:%{path}s', path = Sprintf(path), logLevel = 2);
	lib = .libPaths()[n];
	writeTable(Df(pkg = row.names(installed.packages(lib))), path = Sprintf(path));
}
ReadPackageNames = function(path = '~/Documents/AdminComputer/R/packages-%{version}s.csv',
	version = R.Version()$version.string) {
	Logs('Reading package names to file:%{path}s', path = Sprintf(path), logLevel = 2);
	readTable(Sprintf(path))$pkg;
}
ReinstallPackages = function(packages, n = 1, type = 'source') {
	lib = lib <- .libPaths()[n];
	install.packages(lib  = lib, pkgs = packages, type = type)
}
Reinstall = function(n = 1, type = 'source') {
	ReinstallPackages(row.names(installed.packages(lib)), n = n , type = type);
}
ReinstallFromVersion = function(version, n = 1, type = 'source') {
	ReinstallPackages(ReadPackageNames(version = version), n = n , type = type);
}

#
#	<p> misc linux system stuff
#

agenda = function()system('bash -l', input = c('shopt -s expand_aliases', 'agenda'))
agenda_stop = function()system('bash -l', input = c('shopt -s expand_aliases', 'agenda-stop'))
# 10.2.2020: re-implemented in RtestingHelpers.R
#runTests = function()system('RrunTests')

#
#	<p> random numbers
#

#
#	<p> Reporting
#

# Knit('dir/input.Rmd')
Knit = function(input, output = NULL, ..., format = 'html') {
	Require('knitr');
	Require('markdown');
	sp = splitPath(input);
# 	knit(input, output = with(sp, Sprintf('%{fullbase}s.md')));
# 	switch(format,
# 		'html' = markdownToHTML(with(sp, Sprintf('%{fullbase}s.md')), with(sp, Sprintf('%{fullbase}s.html')))
# 	)
	owd = getwd();
	setwd(sp$dir);
	for (f in format) {
		switch(f,
			'html' = rmarkdown::render(sp$file,
				output_format = rmarkdown::html_document(),
				output_file = with(sp, Sprintf('%{base}s.html')), ...)
		)
	}
	setwd(owd);
}

#
#	<p> stop
#

#
#	<p> debugging
#

#
#	<p> file system
#

#
#	<p> IO
#

sinkStop = function(env = parent.frame()) {
	if (get('sinkOpen', envir = env)) {
		sink(type = 'output');
		sink(type = 'message');
	}
	assign('sinkOpen', FALSE, envir = env)
}

Capture = function(expr, tee = TRUE, envir = parent.frame()) {
	# <p> setup
	tO = tempfile()
	tE = tempfile()
	sink(file(tO, open = 'w'), type = 'output', split = tee);
	sink(file(tE, open = 'w'), type = 'message', split = FALSE);
	sinkOpen = TRUE;
	on.exit( sinkStop() );

	# <p> evaluate
	r = eval(expr, envir = envir);
	sinkStop();

	# <p> read output, print, return
	rCap = list(return = r, stdout = readFile(tO), stderr = readFile(tE));
	if (tee) cat(rCap$stderr, file = 'stderr');
	return(rCap);
}


#
#	<p> low-level
#

renice = function(pid = Sys.getpid(), nicity = 20, logLevel = 4) {
	SystemS('renice %{nicity}d %{pid}d', logLevel);
}

#
#	<p> encryption
#

# convert string to fixed length int vector
secret2key = function(secret, len = 32) {
	keyC = as.integer(charToRaw(join(c(secret, sha256(secret)))));
	L = length(keyC);
	Nblocks = (L + len - 1) %/% len;
	key = integer(length = len);
	for (i in Nblocks) { for (j in 1:len) {
		I = (i - 1) * len + j;
		key[j] = bitwXor(key[j], if (I > L) 0 else keyC[I]);
	} }
	return(as.raw(key))
}

# plain tb of class raw
encryptRaw = function(plain, secret)aes_cbc_encrypt(plain, secret2key(secret))
# returns vector of class raw
decryptRaw = function(message, secret)aes_cbc_decrypt(message, secret2key(secret))

encryptString = function(plain, secret)encryptRaw(charToRaw(plain), secret)
decryptString = function(message, secret)rawToChar(decryptRaw(message, secret))
encryptFile = function(pathIn, pathOut, secret, reader = readChar, writer = writeChar)
	writeFile(pathOut, encryptString(readFile(pathIn, reader = reader), secret = secret), writer = writer)
decryptFile = function(pathIn, pathOut, secret, reader = readChar, writer = writeChar)
	writeFile(pathOut, decryptString(readFile(pathIn, reader = reader), secret = secret), writer = writer)
