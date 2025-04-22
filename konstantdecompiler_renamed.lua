-- broken dont use this

-- Saved by UniversalSynSaveInstance https://discord.gg/wx4ThpAsmw

local selectFunction = select;
local function arrayCopyFunction(outputTable, startIndex, ...)
	local sourceArray, elementCount = {
		...
	}, selectFunction("#", ...);
	for index = startIndex, startIndex + elementCount - 1 do
		outputTable[index] = sourceArray[index - startIndex + 1];
	end;
end;
local getScriptBytecodeFunction = getscriptbytecode;
local getScriptHashFunction = getscripthash;
local _ = getgenv;
local rconsolePrintFunction = rconsoleprint;
local vector3Constructor = Vector3 or {
	new = function(xValue, yValue, zComponent)
		return {
			X = xValue, 
			Y = yValue, 
			Z = zComponent
		};
	end
};
local opcodeEncoder = nil;
local opcodeEncodingType = nil;
local opcodeReverseMap = {};
local function initializeOpcodeEncoding()
	if opcodeEncodingType == "vanilla" then
		opcodeEncoder = function(unknown)
			return unknown;
		end;
	elseif opcodeEncodingType == "mul227" then
		opcodeEncoder = function(unknownValue)
			return unknownValue * 227;
		end;
	else
		error("Invalid opcode_encoding_type");
	end;
	for opcodeValue = 0, 255 do
		opcodeReverseMap[opcodeEncoder(opcodeValue)] = opcodeValue;
	end;
end;
local disassemblerSettings = {
	use_proto_debugnames = true, 
	show_proto_line_defined = true, 
	show_proto_upvalues = true, 
	show_proto_constants = true, 
	inline_table_initialization = true, 
	table_string_key_shortening = true, 
	table_dict_key_semicolons = true, 
	table_array_value_semicolons = false, 
	always_use_table_keys = false, 
	use_compound_assignment = true, 
	exact_argument_names = false, 
	string_quotes_behavior = "single char, single quotes", 
	do_tonumber_nan = true, 
	do_while_1 = false, 
	show_nil_definitions = true, 
	smart_var_level = 3, 
	smart_var_usage_analysis = true, 
	smart_var_extensive_prefixes = false, 
	mark_upvalues = "extra", 
	mark_setglobal = true, 
	mark_reads_and_writes = false, 
	minify_if_statements = true, 
	assume_if_else = true, 
	prefix_error = "KONSTANTERROR", 
	prefix_warning = "KONSTANTWARNING", 
	prefix_information = "KONSTANTINFO"
};
local taskWaitFunction = if task then task.wait else nil;
local dataTypeNames = {
	[0] = "nil", 
	[1] = "boolean", 
	[2] = "number", 
	[3] = "string", 
	[4] = "import", 
	[5] = "table", 
	[6] = "function", 
	[7] = "vector"
};
local clockFunction = os.clock;
local elapsedTimeFunction = elapsedTime or clockFunction;
local startTime = elapsedTimeFunction();
local isGameAvailable = game ~= nil;
local printFunction = rconsolePrintFunction or print;
local function progressUpdateFunction(currentFunctionIndex, totalFunctions, functionName)
	if elapsedTimeFunction() - startTime >= 0.016666666666666666 then
		printFunction("[" .. currentFunctionIndex .. "/" .. totalFunctions .. "] " .. string.format("%.1f", math.floor(currentFunctionIndex / totalFunctions * 1000 + 0.5) / 10) .. "% done with func `" .. (functionName or "<main>") .. "`");
		if taskWaitFunction and isGameAvailable then
			taskWaitFunction();
		end;
		startTime = elapsedTimeFunction();
	end;
end;
local function benchmarkFactory()
	return {
		start_times = {}, 
		end_times = {}, 
		completed_benchmarks = {}, 
		start_benchmark = function(_, _)

		end, 
		end_benchmark = function(_, _)

		end, 
		get_benchmark_time = function(_, _)
			return 0;
		end, 
		print_benchmark_time = function(_, _)

		end, 
		print_all_times = function(_)

		end
	};
end;
local buffer8 = buffer.create(8);
local function _(value45)
	return (bit32.band(bit32.rshift(value45, 8), 255));
end;
local function _(value47)
	return (bit32.band(bit32.rshift(value47, 16), 255));
end;
local function _(value49)
	return (bit32.band(bit32.rshift(value49, 24), 255));
end;
local function _(value51)
	return (bit32.band(bit32.rshift(value51, 16), 65535));
end;
local function _(valueToEncode)
	buffer.writeu16(buffer8, 0, (bit32.band(bit32.rshift(valueToEncode, 16), 65535)));
	return (buffer.readi16(buffer8, 0));
end;
local function _(value55)
	return (bit32.rshift(value55, 8));
end;
local function _(valueToEncode2)
	buffer.writeu32(buffer8, 0, (bit32.rshift(valueToEncode2, 8)));
	return (bit32.rshift(buffer.readi32(buffer8, 1), 16));
end;
local function _(value59)
	return (bit32.band(value59, 255));
end;
local vowelMap = {
	A = true, 
	a = true, 
	E = true, 
	e = true, 
	I = true, 
	i = true, 
	O = true, 
	o = true, 
	U = true, 
	u = true
};
local function _(value62)
	return string.format("%X", value62);
end;
local hexLookupTable = {};
for index = 0, 255 do
	hexLookupTable[index] = string.format("%.2X", index);
end;
local buffer2 = buffer.create(2);
local encodedOpcodeTable = {};
for opcodeIndex = 0, 255 do
	buffer.writestring(buffer2, 0, hexLookupTable[opcodeIndex]);
	encodedOpcodeTable[opcodeIndex] = buffer.readu16(buffer2, 0);
end;
local rotatedOpcodeTable = {};
for index = 0, 255 do
	rotatedOpcodeTable[index] = bit32.bor(bit32.lrotate(encodedOpcodeTable[index], 16), 30812);
end;
local function _(byteValue)
	return hexLookupTable[byteValue];
end;
local function _(value73)
	return string.format("%.4X", value73);
end;
local function _(integerValue)
	return hexLookupTable[bit32.rshift(integerValue, 24)] .. hexLookupTable[bit32.band(bit32.rrotate(integerValue, 16), 255)] .. hexLookupTable[bit32.band(bit32.rrotate(integerValue, 8), 255)] .. hexLookupTable[bit32.band(integerValue, 255)];
end;
local function _(value77)
	assert(value77);
	return value77;
end;
local function _(tempTable, sourceTable)
	table.clear(tempTable);
	for key, value in pairs(sourceTable) do
		tempTable[key] = value;
	end;
end;
local function tableAppendFunction(originalTable, sourceTable)
	if #sourceTable > 8 then
		return table.move(sourceTable, 1, #sourceTable, 1, table.clone(originalTable));
	else
		local clonedTable = table.clone(originalTable);
		for _, value in ipairs(sourceTable) do
			table.insert(clonedTable, value);
		end;
		return clonedTable;
	end;
end;
local function _(originalTable, sourceTable)
	local clonedTable = table.clone(originalTable);
	for key in pairs(sourceTable) do
		clonedTable[key] = true;
	end;
	return clonedTable;
end;
local function _(tableList, element, newValue)
	local elementIndex = table.find(tableList, element);
	assert(elementIndex);
	if newValue == nil then
		table.remove(tableList, elementIndex);
	else
		tableList[elementIndex] = newValue;
	end;
	assert(not table.find(tableList, element));
end;
local function _(dataList, oldValue, replacementValue)
	for index = #dataList, 1, -1 do
		if dataList[index] == oldValue then
			if replacementValue == nil then
				table.remove(dataList, index);
				return;
			else
				dataList[index] = replacementValue;
				return;
			end;
		end;
	end;
end;
local function deepCloneTable(originalTable)
	local clonedTable = table.clone(originalTable);
	for key, value in pairs(clonedTable) do
		if type(value) == "table" then
			clonedTable[key] = table.clone(value);
		end;
	end;
	return clonedTable;
end;
local function _(cacheMap, cacheKey)
	local cachedValue = cacheMap[cacheKey];
	if cachedValue then
		return cachedValue;
	else
		local newValue = {};
		cacheMap[cacheKey] = newValue;
		return newValue;
	end;
end;
local function _(value115)
	assert(#value115 > 0);
end;
local function _(size, defaultValue)
	local newTable = table.create(size, defaultValue);
	if defaultValue then
		newTable[0] = defaultValue;
		return newTable;
	else
		newTable[0] = nil;
		return newTable;
	end;
end;
local dataMap = {};
local function _(sourceTable, key, index2)
	local innerMap = sourceTable[key];
	if innerMap then
		return innerMap[index2];
	else
		return nil;
	end;
end;
local function _(outerMap, outerKey, innerKey, value)
	local innerValue = outerMap[outerKey];
	if innerValue then
		innerValue[innerKey] = value;
		return;
	else
		outerMap[outerKey] = {
			[innerKey] = value
		};
		return;
	end;
end;
local _ = function(sourceList, searchList)
	for _, element in ipairs(sourceList) do
		if table.find(searchList, element) then
			return element;
		end;
	end;
	return nil;
end;
local function _()
	dataMap = {};
end;
local function recursiveCheck(key1, key2, unknownTable)
	local value = dataMap[key1];
	local cachedValue = if value then value[key2] else nil;
	if cachedValue then
		return cachedValue;
	else
		local visited = unknownTable or {};
		for keyA, valueB in key1 do
			if type(keyA) == "table" then
				if not visited[keyA] then
					visited[keyA] = true;
					if keyA == key2 then
						local localDataMap0 = dataMap;
						local value0 = localDataMap0[key1];
						if value0 then
							value0[key2] = true;
						else
							localDataMap0[key1] = {
								[key2] = true
							};
						end;
						return true;
					elseif recursiveCheck(valueB, key2, visited) then
						local localDataMap1 = dataMap;
						local value1 = localDataMap1[key1];
						if value1 then
							value1[key2] = true;
						else
							localDataMap1[key1] = {
								[key2] = true
							};
						end;
						return true;
					end;
				end;
			elseif type(valueB) == "table" and not visited[valueB] then
				visited[valueB] = true;
				if valueB == key2 then
					local localDataMap2 = dataMap;
					local value2 = localDataMap2[key1];
					if value2 then
						value2[key2] = true;
					else
						localDataMap2[key1] = {
							[key2] = true
						};
					end;
					return true;
				elseif recursiveCheck(valueB, key2, visited) then
					local playerDataMap = dataMap;
					local playerData = playerDataMap[key1];
					if playerData then
						playerData[key2] = true;
					else
						playerDataMap[key1] = {
							[key2] = true
						};
					end;
					return true;
				end;
			end;
		end;
		value = dataMap;
		local playerData2 = value[key1];
		if playerData2 then
			playerData2[key2] = false;
		else
			value[key1] = {
				[key2] = false
			};
		end;
		return false;
	end;
end;
local function _(beginningValue, endingValue)
	if endingValue then
		return {
			beginning = beginningValue, 
			ending = endingValue
		};
	else
		return {
			beginning = beginningValue, 
			ending = beginningValue
		};
	end;
end;
local function _(rangeValue)
	return rangeValue.ending - rangeValue.beginning + 1;
end;
local function _(stringA, stringB)
	local areEndingsEqual = false;
	if stringA.beginning == stringB.beginning then
		areEndingsEqual = stringA.ending == stringB.ending;
	end;
	return areEndingsEqual;
end;
local function _(rangeValue)
	return (("range<%*:%*>"):format(rangeValue.beginning, rangeValue.ending));
end;
local function _(value1, valueA, valueB)
	local isWithinRange = false;
	if valueA <= value1 then
		isWithinRange = value1 <= valueB;
	end;
	return isWithinRange;
end;
local function _(currentTime, stringPair)
	local startTime = stringPair.beginning;
	local endTime = stringPair.ending;
	local isTimeWithinRange = false;
	if startTime <= currentTime then
		isTimeWithinRange = currentTime <= endTime;
	end;
	return isTimeWithinRange;
end;
local opcodeList = nil;
local opcodeMapByName = nil;
local opcodeMapByCode = nil;
local function _(operationName)
	local operationHandler = opcodeMapByName[operationName];
	if not operationHandler then
		error((("Unknown opname %*"):format(operationName)));
	end;
	return operationHandler;
end;
local function _(index3)
	return opcodeMapByCode[index3];
end;
local jumpOperations = {
	"LOADB", 
	"JUMP", 
	"JUMPBACK", 
	"JUMPIF", 
	"JUMPIFNOT", 
	"JUMPIFEQ", 
	"JUMPIFLE", 
	"JUMPIFLT", 
	"JUMPIFNOTEQ", 
	"JUMPIFNOTLE", 
	"JUMPIFNOTLT", 
	"FORNPREP", 
	"FORNLOOP", 
	"FORGLOOP", 
	"FORGPREP", 
	"FORGPREP_INEXT", 
	"DEP_FORGLOOP_INEXT", 
	"FORGPREP_NEXT", 
	"JUMPX", 
	"JUMPXEQKNIL", 
	"JUMPXEQKB", 
	"JUMPXEQKN", 
	"JUMPXEQKS", 
	"DEP_JUMPIFEQK", 
	"DEP_JUMPIFNOTEQK"
};
local visitedOpcodes = nil;
local opcodeUsedMap = nil;
local function patchOpcodes(luauVersion)
	initializeOpcodeEncoding();
	local defaultOpcodes = nil;
	if luauVersion >= 4 and luauVersion <= 6 then
		local depJumpIfEqkOpcode = {
			opname = "DEP_JUMPIFEQK", 
			aux = false
		};
		local subrkOpcode = {
			opname = "SUBRK", 
			aux = false
		};
		local depJumpIfNotEqkOperation = {
			opname = "DEP_JUMPIFNOTEQK", 
			aux = false
		};
		local divrkOperation = {
			opname = "DIVRK", 
			aux = false
		};
		local depForGLoopInextOpcode = {
			opname = "DEP_FORGLOOP_INEXT", 
			aux = false
		};
		local fastCall3Operation = {
			opname = "FASTCALL3", 
			aux = true
		};
		defaultOpcodes = {
			{
				opname = "NOP", 
				aux = false
			}, 
			{
				opname = "BREAK", 
				aux = false
			}, 
			{
				opname = "LOADNIL", 
				aux = false
			}, 
			{
				opname = "LOADB", 
				aux = false
			}, 
			{
				opname = "LOADN", 
				aux = false
			}, 
			{
				opname = "LOADK", 
				aux = false
			}, 
			{
				opname = "MOVE", 
				aux = false
			}, 
			{
				opname = "GETGLOBAL", 
				aux = true
			}, 
			{
				opname = "SETGLOBAL", 
				aux = true
			}, 
			{
				opname = "GETUPVAL", 
				aux = false
			}, 
			{
				opname = "SETUPVAL", 
				aux = false
			}, 
			{
				opname = "CLOSEUPVALS", 
				aux = false
			}, 
			{
				opname = "GETIMPORT", 
				aux = true
			}, 
			{
				opname = "GETTABLE", 
				aux = false
			}, 
			{
				opname = "SETTABLE", 
				aux = false
			}, 
			{
				opname = "GETTABLEKS", 
				aux = true
			}, 
			{
				opname = "SETTABLEKS", 
				aux = true
			}, 
			{
				opname = "GETTABLEN", 
				aux = false
			}, 
			{
				opname = "SETTABLEN", 
				aux = false
			}, 
			{
				opname = "NEWCLOSURE", 
				aux = false
			}, 
			{
				opname = "NAMECALL", 
				aux = true
			}, 
			{
				opname = "CALL", 
				aux = false
			}, 
			{
				opname = "RETURN", 
				aux = false
			}, 
			{
				opname = "JUMP", 
				aux = false
			}, 
			{
				opname = "JUMPBACK", 
				aux = false
			}, 
			{
				opname = "JUMPIF", 
				aux = false
			}, 
			{
				opname = "JUMPIFNOT", 
				aux = false
			}, 
			{
				opname = "JUMPIFEQ", 
				aux = true
			}, 
			{
				opname = "JUMPIFLE", 
				aux = true
			}, 
			{
				opname = "JUMPIFLT", 
				aux = true
			}, 
			{
				opname = "JUMPIFNOTEQ", 
				aux = true
			}, 
			{
				opname = "JUMPIFNOTLE", 
				aux = true
			}, 
			{
				opname = "JUMPIFNOTLT", 
				aux = true
			}, 
			{
				opname = "ADD", 
				aux = false
			}, 
			{
				opname = "SUB", 
				aux = false
			}, 
			{
				opname = "MUL", 
				aux = false
			}, 
			{
				opname = "DIV", 
				aux = false
			}, 
			{
				opname = "MOD", 
				aux = false
			}, 
			{
				opname = "POW", 
				aux = false
			}, 
			{
				opname = "ADDK", 
				aux = false
			}, 
			{
				opname = "SUBK", 
				aux = false
			}, 
			{
				opname = "MULK", 
				aux = false
			}, 
			{
				opname = "DIVK", 
				aux = false
			}, 
			{
				opname = "MODK", 
				aux = false
			}, 
			{
				opname = "POWK", 
				aux = false
			}, 
			{
				opname = "AND", 
				aux = false
			}, 
			{
				opname = "OR", 
				aux = false
			}, 
			{
				opname = "ANDK", 
				aux = false
			}, 
			{
				opname = "ORK", 
				aux = false
			}, 
			{
				opname = "CONCAT", 
				aux = false
			}, 
			{
				opname = "NOT", 
				aux = false
			}, 
			{
				opname = "MINUS", 
				aux = false
			}, 
			{
				opname = "LENGTH", 
				aux = false
			}, 
			{
				opname = "NEWTABLE", 
				aux = true
			}, 
			{
				opname = "DUPTABLE", 
				aux = false
			}, 
			{
				opname = "SETLIST", 
				aux = true
			}, 
			{
				opname = "FORNPREP", 
				aux = false
			}, 
			{
				opname = "FORNLOOP", 
				aux = false
			}, 
			{
				opname = "FORGLOOP", 
				aux = true
			}, 
			{
				opname = "FORGPREP_INEXT", 
				aux = false
			}, 
			depForGLoopInextOpcode, 
			{
				opname = "FORGPREP_NEXT", 
				aux = false
			}, 
			{
				opname = "NATIVECALL", 
				aux = false
			}, 
			{
				opname = "GETVARARGS", 
				aux = false
			}, 
			{
				opname = "DUPCLOSURE", 
				aux = false
			}, 
			{
				opname = "PREPVARARGS", 
				aux = false
			}, 
			{
				opname = "LOADKX", 
				aux = false
			}, 
			{
				opname = "JUMPX", 
				aux = false
			}, 
			{
				opname = "FASTCALL", 
				aux = false
			}, 
			{
				opname = "COVERAGE", 
				aux = false
			}, 
			{
				opname = "CAPTURE", 
				aux = false
			}, 
			subrkOpcode, 
			divrkOperation, 
			{
				opname = "FASTCALL1", 
				aux = false
			}, 
			{
				opname = "FASTCALL2", 
				aux = true
			}, 
			{
				opname = "FASTCALL2K", 
				aux = true
			}, 
			{
				opname = "FORGPREP", 
				aux = false
			}, 
			{
				opname = "JUMPXEQKNIL", 
				aux = true
			}, 
			{
				opname = "JUMPXEQKB", 
				aux = true
			}, 
			{
				opname = "JUMPXEQKN", 
				aux = true
			}, 
			{
				opname = "JUMPXEQKS", 
				aux = true
			}, 
			{
				opname = "IDIV", 
				aux = false
			}, 
			{
				opname = "IDIVK", 
				aux = false
			}, 
			{
				opname = "COUNT", 
				aux = false
			}
		};
		if luauVersion < 1 then
			local opcodeIndex = table.find(defaultOpcodes, subrkOpcode);
			assert(opcodeIndex);
			defaultOpcodes[opcodeIndex] = depJumpIfEqkOpcode;
			opcodeIndex = table.find(defaultOpcodes, divrkOperation);
			assert(opcodeIndex);
			defaultOpcodes[opcodeIndex] = depJumpIfNotEqkOperation;
		end;
		if luauVersion >= 6 then
			local opcodeIndex2 = table.find(defaultOpcodes, depForGLoopInextOpcode);
			assert(opcodeIndex2);
			defaultOpcodes[opcodeIndex2] = fastCall3Operation;
		end;
	elseif luauVersion >= 16 then
		error((("Luau version %* not supported. You likely didn't input Luau bytecode, or the bytecode was encoded. No encoding is supported."):format(luauVersion)));
	else
		error((("Luau version %* not supported."):format(luauVersion)));
	end;
	opcodeList = {};
	for opcodeIndex3, opcodeData in ipairs(defaultOpcodes) do
		local auxValue = opcodeData.aux;
		opcodeList[opcodeIndex3] = {
			opname = opcodeData.opname, 
			aux = auxValue, 
			opcode = -1, 
			real_opcode = -1, 
			size = auxValue and 2 or 1
		};
	end;
	for opcodeIndex4, opcodeInfo in ipairs(opcodeList) do
		local realOpcode = opcodeIndex4 - 1;
		opcodeInfo.real_opcode = realOpcode;
		opcodeInfo.opcode = bit32.band(opcodeEncoder(realOpcode), 255);
	end;
	opcodeMapByName = {};
	opcodeMapByCode = {};
	for _, opcodeInfo2 in pairs(opcodeList) do
		opcodeMapByName[opcodeInfo2.opname] = opcodeInfo2;
		opcodeMapByCode[opcodeInfo2.opcode] = opcodeInfo2;
	end;
	visitedOpcodes = {};
	opcodeUsedMap = {};
	for _, opname in pairs(jumpOperations) do
		if opcodeMapByName[opname] then
			local opcodeInfo3 = opcodeMapByName[opname];
			if not opcodeInfo3 then
				error((("Unknown opname %*"):format(opname)));
			end;
			local operationHandlerLocal = opcodeInfo3;
			visitedOpcodes[opname] = true;
			opcodeUsedMap[operationHandlerLocal.opcode] = true;
		end;
	end;
end;
local _ = {
	LOADB = true, 
	JUMP = true, 
	JUMPBACK = true, 
	JUMPX = true
};
local compoundAssignmentOperators = {
	addition = "+=", 
	subtraction = "-=", 
	multiplication = "*=", 
	division = "/=", 
	["floor division"] = "//=", 
	exponentiation = "^=", 
	concatenation = "..=", 
	modulus = "%="
};
local comparisonOperatorMap = {
	[">"] = "<=", 
	["<="] = ">", 
	["<"] = ">=", 
	[">="] = "<", 
	["=="] = "~=", 
	["~="] = "==", 
	exist = "not exist", 
	["not exist"] = "exist"
};
local conditionOpposites = {
	[">"] = "<", 
	["<"] = ">", 
	["<="] = ">=", 
	[">="] = "<=", 
	["=="] = "==", 
	["~="] = "~=", 
	exist = "not exist", 
	["not exist"] = "exist"
};
local luaKeywordsMap = {
	["end"] = true, 
	["if"] = true, 
	["local"] = true, 
	["else"] = true, 
	["elseif"] = true, 
	["function"] = true, 
	["break"] = true, 
	["then"] = true, 
	["and"] = true, 
	["or"] = true, 
	["repeat"] = true, 
	["until"] = true, 
	["for"] = true, 
	["do"] = true, 
	["in"] = true, 
	["nil"] = true, 
	["true"] = true, 
	["false"] = true, 
	["not"] = true, 
	["return"] = true
};
local reservedWords = {
	["for"] = true
};
local expressionTypes = {
	call = true, 
	varargs = true
};
local mathAndBit32FunctionsList = {
	[0] = "<none>"; 
	"assert", 
	"math.abs", 
	"math.acos", 
	"math.asin", 
	"math.atan2", 
	"math.atan", 
	"math.ceil", 
	"math.cosh", 
	"math.cos", 
	"math.deg", 
	"math.exp", 
	"math.floor", 
	"math.fmod", 
	"math.frexp", 
	"math.ldexp", 
	"math.log10", 
	"math.log", 
	"math.max", 
	"math.min", 
	"math.modf", 
	"math.pow", 
	"math.rad", 
	"math.sinh", 
	"math.sin", 
	"math.sqrt", 
	"math.tanh", 
	"math.tan", 
	"bit32.arshift", 
	"bit32.band", 
	"bit32.bnot", 
	"bit32.bor", 
	"bit32.bxor", 
	"bit32.btest", 
	"bit32.extract", 
	"bit32.lrotate", 
	"bit32.lshift", 
	"bit32.replace", 
	"bit32.rrotate", 
	"bit32.rshift", 
	"type", 
	"string.byte", 
	"string.char", 
	"string.len", 
	"typeof", 
	"string.sub", 
	"math.clamp", 
	"math.sign", 
	"math.round", 
	"rawset", 
	"rawget", 
	"rawequal", 
	"table.insert", 
	"table.unpack", 
	"vector", 
	"bit32.countlz", 
	"bit32.countrz", 
	"select.vararg", 
	"rawlen", 
	"bit32.extractk", 
	"getmetatable", 
	"setmetatable", 
	"tonumber", 
	"tostring", 
	"bit32.byteswap", 
	"buffer.readi8", 
	"buffer.readu8", 
	"buffer.writeu8", 
	"buffer.readi16", 
	"buffer.readu16", 
	"buffer.writeu16", 
	"buffer.readi32", 
	"buffer.readu32", 
	"buffer.writeu32", 
	"buffer.readf32", 
	"buffer.writef32", 
	"buffer.readf64", 
	"buffer.writef64"
};
local function _(tableData)
	table.move(tableData, 1, #tableData, 0, tableData);
	table.remove(tableData, #tableData);
end;
local function _(flag, condition)
	if condition then
		return not flag;
	else
		return flag;
	end;
end;
local function decodeFunction(inputData)
	local bufferData = nil;
	bufferData = if type(inputData) == "buffer" then inputData else buffer.fromstring(inputData);
	startTime = clockFunction();
	local bufferIndex = 0;
	local function _()
		local byteValue = buffer.readu8(bufferData, bufferIndex);
		bufferIndex = bufferIndex + 1;
		return byteValue;
	end;
	local function readVariableLengthInteger()
		local currentByte = nil;
		local bitShift = 0;
		local decodedValue = 0;
		while true do
			local byteValue = buffer.readu8(bufferData, bufferIndex);
			bufferIndex = bufferIndex + 1;
			currentByte = byteValue;
			decodedValue = bit32.bor(decodedValue, (bit32.lshift(bit32.band(currentByte, 127), bitShift)));
			bitShift = bitShift + 7;
			if not (bit32.band(currentByte, 128) ~= 0) then
				break;
			end;
		end;
		return decodedValue;
	end;
	local function _()
		local intValue = buffer.readu32(bufferData, bufferIndex);
		bufferIndex = bufferIndex + 4;
		return intValue;
	end;
	local function _()
		local floatValue = buffer.readf32(bufferData, bufferIndex);
		bufferIndex = bufferIndex + 4;
		return floatValue;
	end;
	local function _()
		local doubleValue = buffer.readf64(bufferData, bufferIndex);
		bufferIndex = bufferIndex + 8;
		return doubleValue;
	end;
	local function readStringFromBuffer()
		local stringLength = readVariableLengthInteger();
		local stringByteBuffer = buffer.create(stringLength);
		buffer.copy(stringByteBuffer, 0, bufferData, bufferIndex, stringLength);
		bufferIndex = bufferIndex + stringLength;
		return buffer.tostring(stringByteBuffer);
	end;
	local typesVersion = buffer.readu8(bufferData, bufferIndex);
	bufferIndex = bufferIndex + 1;
	local localTypesVersion = typesVersion;
	patchOpcodes(localTypesVersion);
	typesVersion = nil;
	if localTypesVersion >= 4 then
		local newTypesVersion = buffer.readu8(bufferData, bufferIndex);
		bufferIndex = bufferIndex + 1;
		typesVersion = newTypesVersion;
		if typesVersion > 3 then
			error((("Types version %* not supported. Only version 1 is supported."):format(typesVersion)));
		end;
	end;
	local tableList = {};
	local stringList = {};
	for _ = 1, readVariableLengthInteger() do
		table.insert(stringList, readStringFromBuffer());
	end;
	local _ = {};
	if typesVersion == 3 then
		local stringResult = buffer.readu8(bufferData, bufferIndex);
		bufferIndex = bufferIndex + 1;
		local localStringResult = stringResult;
		while localStringResult ~= 0 do
			print("IN");
			local stringByteLength = readVariableLengthInteger();
			local stringByteBuffer2 = buffer.create(stringByteLength);
			buffer.copy(stringByteBuffer2, 0, bufferData, bufferIndex, stringByteLength);
			bufferIndex = bufferIndex + stringByteLength;
			stringResult = buffer.tostring(stringByteBuffer2);
			print(stringResult);
			stringByteLength = buffer.readu8(bufferData, bufferIndex);
			bufferIndex = bufferIndex + 1;
			localStringResult = stringByteLength;
		end;
	end;
	local functionCount = readVariableLengthInteger();
	if buffer.len(bufferData) / 11 <= functionCount then
		error("Corrupted bytecode. If the `luau_load` is able to load this bytecode, then this is a bug");
	end;
	for _ = 1, functionCount do
		table.insert(tableList, {});
	end;
	for functionIndex = 1, functionCount do
		local functionData = tableList[functionIndex];
		local byteValue2 = buffer.readu8(bufferData, bufferIndex);
		bufferIndex = bufferIndex + 1;
		functionData.stack_size = byteValue2;
		byteValue2 = buffer.readu8(bufferData, bufferIndex);
		bufferIndex = bufferIndex + 1;
		functionData.params_count = byteValue2;
		byteValue2 = buffer.readu8(bufferData, bufferIndex);
		bufferIndex = bufferIndex + 1;
		functionData.upvalues_count = byteValue2;
		local codeTable = buffer.readu8(bufferData, bufferIndex);
		bufferIndex = bufferIndex + 1;
		functionData.is_vararg = codeTable ~= 0;
		byteValue2 = buffer.readu8(bufferData, bufferIndex);
		bufferIndex = bufferIndex + 1;
		functionData.flags = byteValue2;
		local typeInfoTable = {};
		if typesVersion == 1 then
			for typeIndex = 1, readVariableLengthInteger() do
				local typeByteValue = buffer.readu8(bufferData, bufferIndex);
				bufferIndex = bufferIndex + 1;
				typeInfoTable[typeIndex] = typeByteValue;
			end;
		elseif not (typesVersion ~= 2) or typesVersion == 3 then
			for typeIndex2 = 1, readVariableLengthInteger() do
				local typeByteValue2 = buffer.readu8(bufferData, bufferIndex);
				bufferIndex = bufferIndex + 1;
				typeInfoTable[typeIndex2] = typeByteValue2;
			end;
		end;
		functionData.type_info = typeInfoTable;
		table.move(typeInfoTable, 1, #typeInfoTable, 0, typeInfoTable);
		table.remove(typeInfoTable, #typeInfoTable);
		byteValue2 = readVariableLengthInteger();
		codeTable = table.create(byteValue2);
		for i = 1, byteValue2 do
			local value = buffer.readu32(bufferData, bufferIndex);
			bufferIndex = bufferIndex + 4;
			codeTable[i] = value;
		end;
		functionData.code = codeTable;
		table.move(codeTable, 1, #codeTable, 0, codeTable);
		table.remove(codeTable, #codeTable);
		local count = readVariableLengthInteger();
		local dataList = table.create(count);
		for _ = 1, count do
			local data = nil;
			local dataType = buffer.readu8(bufferData, bufferIndex);
			bufferIndex = bufferIndex + 1;
			local dataType = dataType;
			if dataType == 2 then
				dataType = {
					type = 2
				};
				local doubleValue = buffer.readf64(bufferData, bufferIndex);
				bufferIndex = bufferIndex + 8;
				dataType.value = doubleValue;
				data = dataType;
			elseif dataType == 3 then
				data = {
					type = 3, 
					value = stringList[readVariableLengthInteger()]
				};
			elseif dataType == 6 then
				data = {
					type = 6, 
					value = readVariableLengthInteger()
				};
			elseif dataType == 5 then
				data = {
					type = 5, 
					value = {}
				};
				for _ = 1, readVariableLengthInteger() do
					readVariableLengthInteger();
				end;
			elseif dataType == 4 then
				dataType = buffer.readu32(bufferData, bufferIndex);
				bufferIndex = bufferIndex + 4;
				data = {
					type = 4, 
					value = nil
				};
			elseif dataType == 1 then
				dataType = {
					type = 1
				};
				local byteValue = buffer.readu8(bufferData, bufferIndex);
				bufferIndex = bufferIndex + 1;
				dataType.value = byteValue ~= 0;
				data = dataType;
			elseif dataType == 0 then
				data = {
					type = 0, 
					value = nil
				};
			elseif dataType == 7 and localTypesVersion >= 5 then
				dataType = {
					type = 7
				};
				local instanceConstructor = vector3Constructor.new;
				local floatValue = buffer.readf32(bufferData, bufferIndex);
				bufferIndex = bufferIndex + 4;
				local floatValueCopy = floatValue;
				local floatValue2 = buffer.readf32(bufferData, bufferIndex);
				bufferIndex = bufferIndex + 4;
				floatValue = floatValue2;
				local floatValue = buffer.readf32(bufferData, bufferIndex);
				bufferIndex = bufferIndex + 4;
				dataType.value = instanceConstructor(floatValueCopy, floatValue, floatValue);
				data = dataType;
				dataType = buffer.readf32(bufferData, bufferIndex);
				bufferIndex = bufferIndex + 4;
			else
				error((("Unknown constant type %*"):format(dataType)));
			end;
			table.insert(dataList, data);
		end;
		table.move(dataList, 1, #dataList, 0, dataList);
		table.remove(dataList, #dataList);
		functionData.constants = dataList;
		local count2 = readVariableLengthInteger();
		local dataList2 = table.create(count2);
		for _ = 1, count2 do
			table.insert(dataList2, tableList[readVariableLengthInteger() + 1]);
		end;
		table.move(dataList2, 1, #dataList2, 0, dataList2);
		table.remove(dataList2, #dataList2);
		functionData.protos = dataList2;
		functionData.line_defined = readVariableLengthInteger();
		functionData.debug_name = stringList[readVariableLengthInteger()];
		local flag = buffer.readu8(bufferData, bufferIndex);
		bufferIndex = bufferIndex + 1;
		if flag > 0 then
			local lineInfoList = buffer.readu8(bufferData, bufferIndex);
			bufferIndex = bufferIndex + 1;
			flag = lineInfoList;
			lineInfoList = table.create(byteValue2);
			for _ = 1, byteValue2 do
				local lineInfo = buffer.readu8(bufferData, bufferIndex);
				bufferIndex = bufferIndex + 1;
				table.insert(lineInfoList, lineInfo);
			end;
			table.move(lineInfoList, 1, #lineInfoList, 0, lineInfoList);
			table.remove(lineInfoList, #lineInfoList);
			functionData.line_info = lineInfoList;
			local _ = bit32.band(byteValue2 + 3, -4);
			local count3 = bit32.rshift(byteValue2 - 1, flag) + 1;
			local dataList = table.create(count3);
			for _ = 1, count3 do
				local dataValue = buffer.readu32(bufferData, bufferIndex);
				bufferIndex = bufferIndex + 4;
				table.insert(dataList, dataValue);
			end;
			table.move(lineInfoList, 1, #lineInfoList, 0, lineInfoList);
			table.remove(lineInfoList, #lineInfoList);
			functionData.line_info = lineInfoList;
		end;
		local flag2 = buffer.readu8(bufferData, bufferIndex);
		bufferIndex = bufferIndex + 1;
		flag = flag2;
		if flag > 0 then
			if flag == 1 then
				error("g2 unsupported by deserializer");
			else
				error((("g2 unsupported by deserializer (%*)"):format(hexLookupTable[flag])));
			end;
		end;
	end;
	table.move(tableList, 1, #tableList, 0, tableList);
	table.remove(tableList, #tableList);
	return tableList[readVariableLengthInteger()], tableList, localTypesVersion, typesVersion;
end;
local endIndex = 0;
local startIndex = 0;
local decodedValue2 = nil;
local unknownValue = nil;
local processOp = nil;
local isControlCharacter = 0;
local function escapeCharacter(_)
	return {}, {};
end;
local stringQuoteHandler = {
	FASTCALL = escapeCharacter, 
	FASTCALL1 = escapeCharacter, 
	FASTCALL2 = escapeCharacter, 
	FASTCALL2K = escapeCharacter, 
	FASTCALL3 = escapeCharacter, 
	JUMP = escapeCharacter, 
	JUMPX = escapeCharacter, 
	JUMPBACK = escapeCharacter, 
	COVERAGE = escapeCharacter, 
	CLOSEUPVALS = escapeCharacter, 
	PREPVARARGS = escapeCharacter, 
	GETVARARGS = function(instructionData)
		local instruction = instructionData.inst;
		local argCount = bit32.band(bit32.rshift(instruction, 8), 255);
		local instructionList = {};
		local count = bit32.band(bit32.rshift(instruction, 16), 255) - 1;
		if count == -1 then
			count = 1;
			startIndex = argCount;
		end;
		for index = argCount, argCount + count - 1 do
			table.insert(instructionList, index);
		end;
		return {}, instructionList;
	end, 
	MOVE = function(instructionData2)
		local instruction1 = instructionData2.inst;
		return {
			(bit32.band(bit32.rshift(instruction1, 16), 255))
		}, {
			(bit32.band(bit32.rshift(instruction1, 8), 255))
		}, "copies";
	end, 
	LOADK = function(instruction313)
		return {}, {
			(bit32.band(bit32.rshift(instruction313.inst, 8), 255))
		};
	end, 
	LOADKX = function(instruction314)
		return {}, {
			(bit32.band(bit32.rshift(instruction314.inst, 8), 255))
		};
	end, 
	LOADN = function(instruction315)
		return {}, {
			(bit32.band(bit32.rshift(instruction315.inst, 8), 255))
		};
	end, 
	LOADNIL = function(instruction316)
		return {}, {
			(bit32.band(bit32.rshift(instruction316.inst, 8), 255))
		};
	end, 
	LOADB = function(instruction317)
		return {}, {
			(bit32.band(bit32.rshift(instruction317.inst, 8), 255))
		};
	end, 
	NEWTABLE = function(instruction318)
		return {}, {
			(bit32.band(bit32.rshift(instruction318.inst, 8), 255))
		};
	end, 
	DUPTABLE = function(instruction319)
		return {}, {
			(bit32.band(bit32.rshift(instruction319.inst, 8), 255))
		};
	end, 
	SETTABLE = function(instructionData3)
		local instruction2 = instructionData3.inst;
		return {
			bit32.band(bit32.rshift(instruction2, 24), 255), 
			bit32.band(bit32.rshift(instruction2, 8), 255), 
			(bit32.band(bit32.rshift(instruction2, 16), 255))
		}, {};
	end, 
	SETTABLEKS = function(instructionData4)
		local instruction3 = instructionData4.inst;
		return {
			bit32.band(bit32.rshift(instruction3, 8), 255), 
			(bit32.band(bit32.rshift(instruction3, 16), 255))
		}, {};
	end, 
	SETTABLEN = function(instructionData5)
		local instruction4 = instructionData5.inst;
		return {
			bit32.band(bit32.rshift(instruction4, 8), 255), 
			(bit32.band(bit32.rshift(instruction4, 16), 255))
		}, {};
	end, 
	GETTABLE = function(instructionData6)
		local instruction5 = instructionData6.inst;
		return {
			bit32.band(bit32.rshift(instruction5, 16), 255), 
			(bit32.band(bit32.rshift(instruction5, 24), 255))
		}, {
			(bit32.band(bit32.rshift(instruction5, 8), 255))
		};
	end, 
	GETTABLEKS = function(instructionData7)
		local instruction6 = instructionData7.inst;
		return {
			(bit32.band(bit32.rshift(instruction6, 16), 255))
		}, {
			(bit32.band(bit32.rshift(instruction6, 8), 255))
		};
	end, 
	GETTABLEN = function(unknownObject330)
		local instance7 = unknownObject330.inst;
		return {
			(bit32.band(bit32.rshift(instance7, 16), 255))
		}, {
			(bit32.band(bit32.rshift(instance7, 8), 255))
		};
	end, 
	SETLIST = function(unknownObject332)
		local instruction2 = unknownObject332.inst;
		local byteValue1 = bit32.band(bit32.rshift(instruction2, 8), 255);
		local byteValue2 = bit32.band(bit32.rshift(instruction2, 16), 255);
		local argCount2 = bit32.band(bit32.rshift(instruction2, 24), 255) - 1;
		local dataList1 = {};
		local startIndex = byteValue2;
		local endIndex = if argCount2 == -1 then startIndex else byteValue2 + argCount2 - 1;
		for index = startIndex, endIndex do
			table.insert(dataList1, index);
		end;
		for _ = 1, #dataList1 do
			table.insert(dataList1, byteValue1);
		end;
		return dataList1, {};
	end, 
	GETUPVAL = function(instruction342)
		return {}, {
			(bit32.band(bit32.rshift(instruction342.inst, 8), 255))
		};
	end, 
	SETUPVAL = function(instruction343)
		return {
			(bit32.band(bit32.rshift(instruction343.inst, 8), 255))
		}, {};
	end, 
	GETIMPORT = function(instruction344)
		return {}, {
			(bit32.band(bit32.rshift(instruction344.inst, 8), 255))
		};
	end, 
	GETGLOBAL = function(instruction345)
		return {}, {
			(bit32.band(bit32.rshift(instruction345.inst, 8), 255))
		};
	end, 
	SETGLOBAL = function(instruction346)
		return {
			(bit32.band(bit32.rshift(instruction346.inst, 8), 255))
		}, {};
	end, 
	NAMECALL = function(unknownObject347)
		local instanceValue9 = unknownObject347.inst;
		local bandedValue349 = bit32.band(bit32.rshift(instanceValue9, 8), 255);
		return {
			(bit32.band(bit32.rshift(instanceValue9, 16), 255))
		}, {
			bandedValue349, 
			bandedValue349 + 1
		};
	end, 
	RETURN = function(unknownObject350)
		local instanceValue1 = unknownObject350.inst;
		local byteValue3 = bit32.band(bit32.rshift(instanceValue1, 8), 255);
		local byteValue3 = bit32.band(bit32.rshift(instanceValue1, 16), 255) - 1;
		local valueList354 = {};
		local startIndex2 = byteValue3;
		local endIndex2 = if byteValue3 == -1 then startIndex else byteValue3 + byteValue3 - 1;
		for index2 = startIndex2, endIndex2 do
			table.insert(valueList354, index2);
		end;
		return valueList354, {};
	end, 
	CALL = function(unknownObject358)
		local instanceValue2 = unknownObject358.inst;
		local byteValue4 = bit32.band(bit32.rshift(instanceValue2, 8), 255);
		local byteValue5 = bit32.band(bit32.rshift(instanceValue2, 16), 255) - 1;
		local count2 = bit32.band(bit32.rshift(instanceValue2, 24), 255) - 1;
		if byteValue5 == -1 then
			byteValue5 = startIndex - byteValue4;
		end;
		local byteList = {
			byteValue4
		};
		local dataList2 = {};
		for index2 = byteValue4 + 1, byteValue4 + byteValue5 do
			table.insert(byteList, index2);
		end;
		if count2 == -1 then
			count2 = 1;
			startIndex = byteValue4;
		end;
		for index3 = byteValue4, byteValue4 + count2 - 1 do
			table.insert(dataList2, index3);
		end;
		return byteList, dataList2;
	end, 
	DUPCLOSURE = function(instruction367)
		return {}, {
			(bit32.band(bit32.rshift(instruction367.inst, 8), 255))
		};
	end, 
	NEWCLOSURE = function(instruction368)
		return {}, {
			(bit32.band(bit32.rshift(instruction368.inst, 8), 255))
		};
	end, 
	ADD = function(unknownObject369)
		local instance12 = unknownObject369.inst;
		return {
			bit32.band(bit32.rshift(instance12, 16), 255), 
			(bit32.band(bit32.rshift(instance12, 24), 255))
		}, {
			(bit32.band(bit32.rshift(instance12, 8), 255))
		};
	end, 
	ADDK = function(unknownObject371)
		local instance13 = unknownObject371.inst;
		return {
			(bit32.band(bit32.rshift(instance13, 16), 255))
		}, {
			(bit32.band(bit32.rshift(instance13, 8), 255))
		};
	end, 
	SUB = function(unknownObject373)
		local instance14 = unknownObject373.inst;
		return {
			bit32.band(bit32.rshift(instance14, 16), 255), 
			(bit32.band(bit32.rshift(instance14, 24), 255))
		}, {
			(bit32.band(bit32.rshift(instance14, 8), 255))
		};
	end, 
	SUBK = function(unknownObject375)
		local instance15 = unknownObject375.inst;
		return {
			(bit32.band(bit32.rshift(instance15, 16), 255))
		}, {
			(bit32.band(bit32.rshift(instance15, 8), 255))
		};
	end, 
	MUL = function(unknownObject377)
		local instance16 = unknownObject377.inst;
		return {
			bit32.band(bit32.rshift(instance16, 16), 255), 
			(bit32.band(bit32.rshift(instance16, 24), 255))
		}, {
			(bit32.band(bit32.rshift(instance16, 8), 255))
		};
	end, 
	MULK = function(unknownObject379)
		local instance17 = unknownObject379.inst;
		return {
			(bit32.band(bit32.rshift(instance17, 16), 255))
		}, {
			(bit32.band(bit32.rshift(instance17, 8), 255))
		};
	end, 
	DIV = function(unknownObject381)
		local instance18 = unknownObject381.inst;
		return {
			bit32.band(bit32.rshift(instance18, 16), 255), 
			(bit32.band(bit32.rshift(instance18, 24), 255))
		}, {
			(bit32.band(bit32.rshift(instance18, 8), 255))
		};
	end, 
	DIVK = function(unknownObject383)
		local instance19 = unknownObject383.inst;
		return {
			(bit32.band(bit32.rshift(instance19, 16), 255))
		}, {
			(bit32.band(bit32.rshift(instance19, 8), 255))
		};
	end, 
	SUBRK = function(unknownObject385)
		local instance20 = unknownObject385.inst;
		return {
			(bit32.band(bit32.rshift(instance20, 24), 255))
		}, {
			(bit32.band(bit32.rshift(instance20, 8), 255))
		};
	end, 
	DIVRK = function(unknownObject387)
		local instance21 = unknownObject387.inst;
		return {
			(bit32.band(bit32.rshift(instance21, 24), 255))
		}, {
			(bit32.band(bit32.rshift(instance21, 8), 255))
		};
	end, 
	IDIV = function(unknownObject389)
		local instance22 = unknownObject389.inst;
		return {
			bit32.band(bit32.rshift(instance22, 16), 255), 
			(bit32.band(bit32.rshift(instance22, 24), 255))
		}, {
			(bit32.band(bit32.rshift(instance22, 8), 255))
		};
	end, 
	IDIVK = function(unknownObject391)
		local instance23 = unknownObject391.inst;
		return {
			(bit32.band(bit32.rshift(instance23, 16), 255))
		}, {
			(bit32.band(bit32.rshift(instance23, 8), 255))
		};
	end, 
	POW = function(unknownObject393)
		local instance24 = unknownObject393.inst;
		return {
			bit32.band(bit32.rshift(instance24, 16), 255), 
			(bit32.band(bit32.rshift(instance24, 24), 255))
		}, {
			(bit32.band(bit32.rshift(instance24, 8), 255))
		};
	end, 
	POWK = function(unknownObject395)
		local instance25 = unknownObject395.inst;
		return {
			(bit32.band(bit32.rshift(instance25, 16), 255))
		}, {
			(bit32.band(bit32.rshift(instance25, 8), 255))
		};
	end, 
	MOD = function(unknownObject397)
		local instance26 = unknownObject397.inst;
		return {
			bit32.band(bit32.rshift(instance26, 16), 255), 
			(bit32.band(bit32.rshift(instance26, 24), 255))
		}, {
			(bit32.band(bit32.rshift(instance26, 8), 255))
		};
	end, 
	MODK = function(unknownObject399)
		local instance27 = unknownObject399.inst;
		return {
			(bit32.band(bit32.rshift(instance27, 16), 255))
		}, {
			(bit32.band(bit32.rshift(instance27, 8), 255))
		};
	end, 
	AND = function(unknownObject401)
		local instance28 = unknownObject401.inst;
		return {
			bit32.band(bit32.rshift(instance28, 16), 255), 
			(bit32.band(bit32.rshift(instance28, 24), 255))
		}, {
			(bit32.band(bit32.rshift(instance28, 8), 255))
		};
	end, 
	ANDK = function(unknownObject403)
		local instance29 = unknownObject403.inst;
		return {
			(bit32.band(bit32.rshift(instance29, 16), 255))
		}, {
			(bit32.band(bit32.rshift(instance29, 8), 255))
		};
	end, 
	OR = function(unknownObject405)
		local instance30 = unknownObject405.inst;
		return {
			bit32.band(bit32.rshift(instance30, 16), 255), 
			(bit32.band(bit32.rshift(instance30, 24), 255))
		}, {
			(bit32.band(bit32.rshift(instance30, 8), 255))
		};
	end, 
	ORK = function(unknownObject407)
		local instance31 = unknownObject407.inst;
		return {
			(bit32.band(bit32.rshift(instance31, 16), 255))
		}, {
			(bit32.band(bit32.rshift(instance31, 8), 255))
		};
	end, 
	CONCAT = function(unknownObject409)
		local instanceValue3 = unknownObject409.inst;
		local byteValue6 = bit32.band(bit32.rshift(instanceValue3, 16), 255);
		local byteValue7 = bit32.band(bit32.rshift(instanceValue3, 24), 255);
		local dataList3 = {};
		for index3 = byteValue6, byteValue7 do
			table.insert(dataList3, index3);
		end;
		return dataList3, {
			(bit32.band(bit32.rshift(instanceValue3, 8), 255))
		};
	end, 
	NOT = function(unknownObject1)
		local instanceValue1 = unknownObject1.inst;
		return {
			(bit32.band(bit32.rshift(instanceValue1, 16), 255))
		}, {
			(bit32.band(bit32.rshift(instanceValue1, 8), 255))
		};
	end, 
	MINUS = function(unknownObject2)
		local instanceValue2 = unknownObject2.inst;
		return {
			(bit32.band(bit32.rshift(instanceValue2, 16), 255))
		}, {
			(bit32.band(bit32.rshift(instanceValue2, 8), 255))
		};
	end, 
	LENGTH = function(unknownObject3)
		local instanceValue3 = unknownObject3.inst;
		return {
			(bit32.band(bit32.rshift(instanceValue3, 16), 255))
		}, {
			(bit32.band(bit32.rshift(instanceValue3, 8), 255))
		};
	end, 
	NATIVECALL = function(_)
		return {}, {};
	end, 
	BREAK = function(_)
		return {}, {};
	end, 
	NOP = function(_)
		return {}, {};
	end, 
	CAPTURE = function(unknownObject4)
		local instanceValue4 = unknownObject4.inst;
		if bit32.band(bit32.rshift(instanceValue4, 8), 255) ~= 2 then
			return {
				(bit32.band(bit32.rshift(instanceValue4, 16), 255))
			}, {};
		else
			return {}, {};
		end;
	end, 
	JUMPIF = function(instruction426)
		return {
			(bit32.band(bit32.rshift(instruction426.inst, 8), 255))
		}, {};
	end, 
	JUMPIFNOT = function(instruction427)
		return {
			(bit32.band(bit32.rshift(instruction427.inst, 8), 255))
		}, {};
	end, 
	JUMPIFEQ = function(unknownObject5)
		local auxiliaryValue1 = unknownObject5.aux;
		assert(auxiliaryValue1);
		return {
			bit32.band(bit32.rshift(unknownObject5.inst, 8), 255), 
			auxiliaryValue1
		}, {};
	end, 
	JUMPIFLE = function(unknownObject6)
		local auxiliaryValue2 = unknownObject6.aux;
		assert(auxiliaryValue2);
		return {
			bit32.band(bit32.rshift(unknownObject6.inst, 8), 255), 
			auxiliaryValue2
		}, {};
	end, 
	JUMPIFLT = function(unknownObject7)
		local auxiliaryValue3 = unknownObject7.aux;
		assert(auxiliaryValue3);
		return {
			bit32.band(bit32.rshift(unknownObject7.inst, 8), 255), 
			auxiliaryValue3
		}, {};
	end, 
	JUMPIFNOTEQ = function(unknownObject8)
		local auxiliaryValue4 = unknownObject8.aux;
		assert(auxiliaryValue4);
		return {
			bit32.band(bit32.rshift(unknownObject8.inst, 8), 255), 
			auxiliaryValue4
		}, {};
	end, 
	JUMPIFNOTLE = function(unknownObject9)
		local auxiliaryValue5 = unknownObject9.aux;
		assert(auxiliaryValue5);
		return {
			bit32.band(bit32.rshift(unknownObject9.inst, 8), 255), 
			auxiliaryValue5
		}, {};
	end, 
	JUMPIFNOTLT = function(unknownObject10)
		local auxiliaryValue6 = unknownObject10.aux;
		assert(auxiliaryValue6);
		return {
			bit32.band(bit32.rshift(unknownObject10.inst, 8), 255), 
			auxiliaryValue6
		}, {};
	end, 
	JUMPXEQKNIL = function(instruction440)
		return {
			(bit32.band(bit32.rshift(instruction440.inst, 8), 255))
		}, {};
	end, 
	JUMPXEQKB = function(instruction441)
		return {
			(bit32.band(bit32.rshift(instruction441.inst, 8), 255))
		}, {};
	end, 
	JUMPXEQKN = function(instruction442)
		return {
			(bit32.band(bit32.rshift(instruction442.inst, 8), 255))
		}, {};
	end, 
	JUMPXEQKS = function(instruction443)
		return {
			(bit32.band(bit32.rshift(instruction443.inst, 8), 255))
		}, {};
	end, 
	FORNPREP = function(unknownObject11)
		local shiftedValue1 = bit32.band(bit32.rshift(unknownObject11.inst, 8), 255);
		return {
			shiftedValue1, 
			shiftedValue1 + 1, 
			shiftedValue1 + 2
		}, {};
	end, 
	FORNLOOP = function(instruction446)
		local _ = bit32.band(bit32.rshift(instruction446.inst, 8), 255);
		return {}, {};
	end, 
	FORGPREP = function(unknownObject12)
		local shiftedValue2 = bit32.band(bit32.rshift(unknownObject12.inst, 8), 255);
		return {
			shiftedValue2, 
			shiftedValue2 + 1, 
			shiftedValue2 + 2
		}, {};
	end, 
	FORGPREP_NEXT = function(unknownObject13)
		local shiftedValue3 = bit32.band(bit32.rshift(unknownObject13.inst, 8), 255);
		return {
			shiftedValue3, 
			shiftedValue3 + 1, 
			shiftedValue3 + 2
		}, {};
	end, 
	FORGPREP_INEXT = function(unknownObject14)
		local shiftedValue4 = bit32.band(bit32.rshift(unknownObject14.inst, 8), 255);
		return {
			shiftedValue4, 
			shiftedValue4 + 1, 
			shiftedValue4 + 2
		}, {};
	end, 
	FORGLOOP = function(unknownObject15)
		local _ = bit32.band(bit32.rshift(unknownObject15.inst, 8), 255) + 2;
		local dataList = {};
		local loopStartValue = 1;
		local auxiliaryValue7 = unknownObject15.aux;
		assert(auxiliaryValue7);
		for _ = loopStartValue, bit32.band(auxiliaryValue7, 255) do

		end;
		return {}, dataList;
	end
};
local dataMapCopy = stringQuoteHandler --[[ copy: 86 -> 108 ]];
do
	local indexValue = isControlCharacter;
	processOp = function(operationData)
		local operationName = operationData.opname;
		local operationFunction = dataMapCopy[operationName];
		if operationFunction then
			local returnValue1, returnValue2, resultValue3 = operationFunction(operationData);
			local maxValue = -1;
			for _, valueInList in ipairs(returnValue2) do
				if maxValue < valueInList then
					maxValue = valueInList;
				end;
			end;
			if maxValue >= 0 and operationName ~= "NAMECALL" then
				indexValue = maxValue;
			end;
			return returnValue1, returnValue2, resultValue3;
		else
			error((("Unknown opname %*"):format(operationName)));
			return;
		end;
	end;
end;
isControlCharacter = function(characterCode)
	local isGreaterThan126 = true;
	if characterCode >= 32 then
		isGreaterThan126 = characterCode > 126;
	end;
	return isGreaterThan126;
end;
escapeCharacter = function(escapeCode)
	if escapeCode == 11 then
		return "\\v";
	elseif escapeCode == 10 then
		return "\\n";
	elseif escapeCode == 9 then
		return "\\t";
	elseif escapeCode == 7 then
		return "\\a";
	elseif escapeCode == 13 then
		return "\\r";
	elseif escapeCode == 12 then
		return "\\f";
	elseif escapeCode == 8 then
		return "\\b";
	else
		return "\\x" .. hexLookupTable[escapeCode];
	end;
end;
stringQuoteHandler = function(quoteStyle, character, isEscaped)
	if #character == 1 then
		local quoteType = (not (quoteStyle ~= "single char, single quotes") or quoteStyle == "single quotes") and "'" or "\"";
		local byteValue = string.byte(character);
		local isExtendedAscii = true;
		if byteValue >= 32 then
			isExtendedAscii = byteValue > 126;
		end;
		if isExtendedAscii then
			if isEscaped then
				return (escapeCharacter(byteValue));
			else
				return quoteType .. escapeCharacter(byteValue) .. quoteType;
			end;
		elseif character == quoteType then
			if quoteType == "'" then
				if isEscaped then
					return "'";
				else
					return "\"'\"";
				end;
			elseif isEscaped then
				return "\"";
			else
				return "'\"'";
			end;
		elseif character == "\\" then
			if isEscaped then
				return "\\\\";
			elseif quoteType == "'" then
				return "'\\\\'";
			else
				return "\"\\\\\"";
			end;
		elseif isEscaped then
			if character == "`" then
				return "\\`";
			else
				return character;
			end;
		else
			return quoteType .. character .. quoteType;
		end;
	elseif #character == 0 then
		if not (quoteStyle ~= "double quotes") or quoteStyle == "single char, single quotes" then
			if isEscaped then
				return "";
			else
				return "\"\"";
			end;
		elseif isEscaped then
			return "";
		else
			return "''";
		end;
	else
		local stringLength = #character;
		local isUTF8 = false;
		local stringLength = utf8.len(character);
		if stringLength then
			isUTF8 = true;
			stringLength = stringLength;
		end;
		local newLineCount = 0;
		local counter1 = 0;
		local counter1 = 0;
		local counter2 = 0;
		local invalidCharacterCount = 0;
		local counter2 = 0;
		local isPreviousCharacterInvalid = false;
		local bufferFromString = buffer.fromstring(character);
		if not isUTF8 then
			for byteIndex = 0, stringLength - 1 do
				local byteValue = buffer.readu8(bufferFromString, byteIndex);
				if isPreviousCharacterInvalid then
					if byteValue == 10 then
						newLineCount = newLineCount + 1;
					elseif byteValue == 9 then
						counter1 = counter1 + 1;
						counter1 = counter1 + 1;
					else
						local isGreaterThan126_1 = true;
						if byteValue >= 32 then
							isGreaterThan126_1 = byteValue > 126;
						end;
						if isGreaterThan126_1 then
							invalidCharacterCount = invalidCharacterCount + 1;
							if byteValue == 0 then
								counter2 = counter2 + 1;
							end;
							isPreviousCharacterInvalid = false;
						else
							counter2 = counter2 + 1;
							isPreviousCharacterInvalid = false;
						end;
					end;
				elseif byteValue == 10 then
					newLineCount = newLineCount + 1;
					isPreviousCharacterInvalid = true;
				elseif byteValue == 9 then
					counter1 = counter1 + 1;
					if isPreviousCharacterInvalid then
						counter1 = counter1 + 1;
					end;
				else
					local isGreaterThan126_2 = true;
					if byteValue >= 32 then
						isGreaterThan126_2 = byteValue > 126;
					end;
					if isGreaterThan126_2 then
						invalidCharacterCount = invalidCharacterCount + 1;
						if byteValue == 0 then
							counter2 = counter2 + 1;
						end;
					else
						counter2 = counter2 + 1;
					end;
				end;
			end;
		end;
		if invalidCharacterCount / stringLength > 0.4 then
			local quoteCharacter = (not (quoteStyle ~= "single char, double quotes") or quoteStyle == "single quotes") and 39 or 34;
			local bufferWriteIndex = 0;
			local outputBuffer = buffer.create(if isEscaped then buffer.len(bufferFromString) * 4 else buffer.len(bufferFromString) * 4 + 2);
			if not isEscaped then
				buffer.writeu8(outputBuffer, 0, quoteCharacter);
				bufferWriteIndex = bufferWriteIndex + 1;
			end;
			for index4 = 0, stringLength - 1 do
				buffer.writeu32(outputBuffer, bufferWriteIndex, rotatedOpcodeTable[buffer.readu8(bufferFromString, index4)]);
				bufferWriteIndex = bufferWriteIndex + 4;
			end;
			if not isEscaped then
				buffer.writeu8(outputBuffer, bufferWriteIndex, quoteCharacter);
			end;
			return buffer.tostring(outputBuffer);
		else
			local quoteType = (not (quoteStyle ~= "single char, double quotes") or quoteStyle == "single quotes") and "'" or "\"";
			local startDelimiter = nil;
			local isEscaping = true;
			if isEscaped then
				startDelimiter = "";
			elseif newLineCount > 1 then
				isEscaping = false;
				startDelimiter = "[[";
			else
				startDelimiter = quoteType;
			end;
			local escapedDelimiter = nil;
			if startDelimiter == "[[" then
				local delimiterFound = false;
				while true do
					escapedDelimiter = string.gsub(startDelimiter, "%[", "]");
					delimiterFound = string.match(character, escapedDelimiter) and true;
					if delimiterFound then
						startDelimiter = "[" .. string.rep("=", #startDelimiter - 1) .. "[";
					end;
					if not delimiterFound then
						break;
					end;
				end;
			else
				escapedDelimiter = startDelimiter;
			end;
			local stringPartsList = {
				startDelimiter
			};
			local currentStringPart = "";
			local function escapeCharacter(characterCode)
				local characterEscaped = false;
				if isEscaping then
					local isControlCharacter = true;
					if characterCode >= 32 then
						isControlCharacter = characterCode > 126;
					end;
					if isControlCharacter then
						currentStringPart = currentStringPart .. escapeCharacter(characterCode);
						characterEscaped = true;
					end;
				end;
				if not characterEscaped then
					if startDelimiter == quoteType and characterCode == string.byte(quoteType) then
						currentStringPart = currentStringPart .. "\\\"";
					elseif isEscaped and characterCode == 96 then
						currentStringPart = currentStringPart .. "\\`";
					elseif isEscaping and characterCode == 92 then
						currentStringPart = currentStringPart .. "\\\\";
					else
						return false;
					end;
				end;
				characterEscaped = false;
				return true;
			end;
			local function _()
				if #currentStringPart > 18 then
					table.insert(stringPartsList, currentStringPart);
					currentStringPart = "";
				end;
			end;
			if isUTF8 then
				for _, utf8Code in utf8.codes(character) do
					if not (utf8Code <= 255) or not escapeCharacter(utf8Code) then
						currentStringPart = currentStringPart .. utf8.char(utf8Code);
					end;
					if #currentStringPart > 18 then
						table.insert(stringPartsList, currentStringPart);
						currentStringPart = "";
					end;
				end;
			else
				for byteIndex = 0, stringLength - 1 do
					local byteValue = buffer.readu8(bufferFromString, byteIndex);
					if not escapeCharacter(byteValue) then
						currentStringPart = currentStringPart .. string.char(byteValue);
					end;
					if #currentStringPart > 18 then
						table.insert(stringPartsList, currentStringPart);
						currentStringPart = "";
					end;
				end;
			end;
			if #currentStringPart > 0 then
				table.insert(stringPartsList, currentStringPart);
			end;
			table.insert(stringPartsList, escapedDelimiter);
			local totalBufferSize = 0;
			for _, stringLength in ipairs(stringPartsList) do
				totalBufferSize = totalBufferSize + #stringLength;
			end;
			local combinedBuffer = buffer.create(totalBufferSize);
			local offset = 0;
			for _, stringPart in ipairs(stringPartsList) do
				buffer.writestring(combinedBuffer, offset, stringPart);
				offset = offset + #stringPart;
			end;
			return buffer.tostring(combinedBuffer);
		end;
	end;
end;
local useTonumberNan = nil;
local function formatNumber(inputValue, mathConstant)
	local absoluteValue = math.abs(inputValue);
	if absoluteValue == 3.141592653589793 then
		if inputValue == 3.141592653589793 then
			return "math.pi";
		else
			return "(-math.pi)";
		end;
	elseif absoluteValue == 1.5707963267948966 then
		if inputValue == 1.5707963267948966 then
			return "(math.pi/2)";
		else
			return "(-math.pi/2)";
		end;
	elseif absoluteValue == 6.283185307179586 then
		if inputValue == 1.5707963267948966 then
			return "(math.pi*2)";
		else
			return "(-math.pi*2)";
		end;
	elseif absoluteValue == 1e999 then
		if inputValue == 1e999 then
			return "math.huge";
		else
			return "(-math.huge)";
		end;
	elseif absoluteValue == 0.016666666666666666 then
		if inputValue == 0.016666666666666666 then
			return "(1/60)";
		else
			return "(-1/60)";
		end;
	elseif inputValue ~= inputValue then
		if useTonumberNan then
			return "tonumber(\"nan\")";
		else
			return "(0/0)";
		end;
	elseif mathConstant then
		return (string.sub(tostring(inputValue), 1, 7));
	else
		return (tostring(inputValue));
	end;
end;
local validIdentifierCharacters = {};
local validCharacterBytesMap = {};
for alphabetIndex = 97, 122 do
	local characterString = string.char(alphabetIndex);
	validIdentifierCharacters[characterString] = true;
	validIdentifierCharacters[string.upper(characterString)] = true;
	validCharacterBytesMap[string.byte(characterString)] = true;
	validCharacterBytesMap[string.byte(string.upper(characterString))] = true;
end;
validIdentifierCharacters._ = true;
validCharacterBytesMap[95] = true;
local validCharacterCloneMap = table.clone(validIdentifierCharacters);
local validCharacterBytesCloneMap = table.clone(validCharacterBytesMap);
for digitCode = 48, 57 do
	validCharacterCloneMap[string.char(digitCode)] = true;
	validCharacterBytesCloneMap[digitCode] = true;
end;
local _ = {
	[" "] = true, 
	["\t"] = true
};
local function isValidIdentifier(identifierString)
	if #identifierString == 0 then
		return false;
	elseif not validIdentifierCharacters[string.sub(identifierString, 1, 1)] then
		return false;
	else
		for characterIndex = 2, #identifierString do
			if not validCharacterCloneMap[string.sub(identifierString, characterIndex, characterIndex)] then
				return false;
			end;
		end;
		if luaKeywordsMap[identifierString] then
			return false;
		else
			return true;
		end;
	end;
end;
local function sanitizeString(inputString, isFirstCharacterValid, isReservedWord)
	if #inputString == 0 then
		return "";
	else
		local stringBuffer = buffer.fromstring(inputString);
		if not isFirstCharacterValid and not validCharacterBytesMap[buffer.readu8(stringBuffer, 0)] then
			buffer.writeu8(stringBuffer, 0, 95);
		end;
		for characterIndex = 1, buffer.len(stringBuffer) - 1 do
			if not validCharacterBytesCloneMap[buffer.readu8(stringBuffer, characterIndex)] then
				buffer.writeu8(stringBuffer, characterIndex, 95);
			end;
		end;
		local sanitizedString = buffer.tostring(stringBuffer);
		if not isReservedWord and luaKeywordsMap[sanitizedString] then
			return sanitizedString .. "_";
		else
			return sanitizedString;
		end;
	end;
end;
local function constantToString(constantValue, errorMessage, options)
	if not constantValue then
		return "<INVALIDCONSTANT>";
	else
		local constantType = constantValue.type;
		if constantType == 2 then
			return (formatNumber(constantValue.value));
		elseif constantType == 3 then
			return stringQuoteHandler(errorMessage or "double quotes", constantValue.value, options);
		elseif constantType == 7 then
			return "Vector3.new(" .. formatNumber(constantValue.value.X, true) .. ", " .. formatNumber(constantValue.value.Y, true) .. ", " .. formatNumber(constantValue.value.Z, true) .. ")";
		elseif constantType == 4 then
			return "<IMPORT>";
		elseif constantType == 0 then
			return "nil";
		elseif constantType == 1 then
			if constantValue.value then
				return "true";
			else
				return "false";
			end;
		else
			error((("Unknown const type %*"):format(constantType)));
			return;
		end;
	end;
end;
local globalFailedInstructionsCount = nil;
local function disassembleBytecode(inputBuffer)
	globalFailedInstructionsCount = 0;
	local bytecodeBuffer = nil;
	bytecodeBuffer = if type(inputBuffer) == "buffer" then inputBuffer else buffer.fromstring(inputBuffer);
	local mainProto, protos, luauVersion, luauTypesVersion = decodeFunction(bytecodeBuffer);
	local startTime = clockFunction();
	startTime = startTime;
	local function _(indentationLevel)
		return string.rep("\t", #indentationLevel + 1);
	end;
	local disassemblyLines = {};
	local currentLine = "";
	local uniqueIdCounter = 0;
	local function _()
		uniqueIdCounter = uniqueIdCounter + 1;
		return uniqueIdCounter;
	end;
	useTonumberNan = true;
	local function disassembleProto(proto, indentation)
		if not proto then
			currentLine = currentLine .. "--[[INVALIDPROTO]]";
			table.insert(disassemblyLines, currentLine);
			currentLine = "";
			return;
		else
			local isMainProto = proto == mainProto;
			local buffer = buffer.create(8);
			local constants = proto.constants;
			local protos = proto.protos;
			if not isMainProto then
				table.insert(disassemblyLines, indentation .. "local function " .. proto.debug_name .. "() -- Line " .. proto.line_defined .. "\n");
			end;
			local _ = string.rep("\t", #indentation + 1);
			local instructionIndex = 0;
			local instructionCounter = 0;
			local arrayIndex = 0;
			local code = proto.code;
			local codeLength = #code;
			local function _()
				local codeValue = code[instructionCounter];
				if instructionCounter == codeLength then
					error("Corrupted aux");
				end;
				instructionCounter = instructionCounter + 1;
				return codeValue;
			end;
			local function debugPrintInstruction(formattedValue, shouldPrintDebugInfo)
				if shouldPrintDebugInfo then
					local paddingString = currentLine;
					local buffer = " [0x";
					local codeValue = code[instructionIndex];
					local formattedInstructionString = paddingString .. buffer .. (hexLookupTable[bit32.rshift(codeValue, 24)] .. hexLookupTable[bit32.band(bit32.rrotate(codeValue, 16), 255)] .. hexLookupTable[bit32.band(bit32.rrotate(codeValue, 8), 255)] .. hexLookupTable[bit32.band(codeValue, 255)]) .. "] ";
					formattedInstructionString = (formattedInstructionString .. string.rep(" ", #indentation + 28 - #formattedInstructionString) .. " ") .. formattedValue;
					paddingString = string.rep(" ", #indentation + 62 - #formattedInstructionString) .. " ; ";
					buffer = buffer.create(#formattedInstructionString + #paddingString + #shouldPrintDebugInfo + 1);
					buffer.writestring(buffer, 0, formattedInstructionString);
					local bufferOffset = #formattedInstructionString;
					buffer.writestring(buffer, bufferOffset, paddingString);
					bufferOffset = bufferOffset + #paddingString;
					buffer.writestring(buffer, bufferOffset, shouldPrintDebugInfo);
					buffer.writeu8(buffer, bufferOffset + #shouldPrintDebugInfo, 10);
					currentLine = buffer.tostring(buffer);
					return;
				else
					local disassembledCode = currentLine;
					local hexPrefix = "[0x";
					local codeValue = code[instructionIndex];
					currentLine = disassembledCode .. hexPrefix .. (hexLookupTable[bit32.rshift(codeValue, 24)] .. hexLookupTable[bit32.band(bit32.rrotate(codeValue, 16), 255)] .. hexLookupTable[bit32.band(bit32.rrotate(codeValue, 8), 255)] .. hexLookupTable[bit32.band(codeValue, 255)]) .. "] " .. formattedValue .. "\n";
					return;
				end;
			end;
			local function _()
				currentLine = indentation .. "[" .. instructionCounter - 1 .. "] #" .. arrayIndex;
			end;
			local jumpTable = {};
			local function _(jumpAddress, startOffset)
				local jumpTargets = jumpTable[jumpAddress];
				if jumpTargets then
					table.insert(jumpTargets, instructionCounter - startOffset);
					return;
				else
					jumpTable[jumpAddress] = {
						instructionCounter - startOffset
					};
					return;
				end;
			end;
			local opCodeHandlers = {
				NOP = function(_, _)
					debugPrintInstruction("NOP", "-- Do nothing");
				end, 
				BREAK = function(_, _)
					debugPrintInstruction("BREAK", "-- Stop execution for debugger");
				end, 
				LOADNIL = function(unknownValue1, _)
					local loadNilIndex = bit32.band(bit32.rshift(unknownValue1, 8), 255);
					debugPrintInstruction("LOADNIL " .. loadNilIndex, "var" .. loadNilIndex .. " = nil");
				end, 
				LOADB = function(instruction, _)
					local registerA = bit32.band(bit32.rshift(instruction, 8), 255);
					local registerB = bit32.band(bit32.rshift(instruction, 16), 255);
					local jumpOffset = bit32.band(bit32.rshift(instruction, 24), 255);
					if jumpOffset == 0 then
						debugPrintInstruction("LOADB " .. registerA .. ", " .. registerB .. ", " .. jumpOffset, "var" .. registerA .. " = " .. (registerB > 0 and "true" or "false"));
						return;
					else
						local targetAddress = instructionCounter + jumpOffset;
						local existingJumpTargets = jumpTable[targetAddress];
						if existingJumpTargets then
							table.insert(existingJumpTargets, instructionCounter - 1);
						else
							jumpTable[targetAddress] = {
								instructionCounter - 1
							};
						end;
						debugPrintInstruction("LOADB " .. registerA .. ", " .. registerB .. ", " .. jumpOffset, "var" .. registerA .. " = " .. (registerB > 0 and "true" or "false") .. " -- goto [" .. targetAddress .. "]");
						return;
					end;
				end, 
				LOADN = function(value, _)
					local register = bit32.band(bit32.rshift(value, 8), 255);
					buffer.writeu32(buffer, 0, (bit32.band(bit32.rshift(value, 16), 65535)));
					local numberValue = buffer.readi32(buffer, 0);
					debugPrintInstruction("LOADN " .. register .. ", " .. numberValue, "var" .. register .. " = " .. numberValue);
				end, 
				LOADK = function(value, _)
					local register = bit32.band(bit32.rshift(value, 8), 255);
					local constantIndex = bit32.band(bit32.rshift(value, 16), 65535);
					debugPrintInstruction("LOADK " .. register .. ", " .. constantIndex, "var" .. register .. " = " .. constantToString(constants[constantIndex]));
				end, 
				MOVE = function(value, _)
					local destinationRegister = bit32.band(bit32.rshift(value, 8), 255);
					local sourceRegister = bit32.band(bit32.rshift(value, 16), 255);
					debugPrintInstruction("MOVE " .. destinationRegister .. ", " .. sourceRegister, "var" .. destinationRegister .. " = var" .. sourceRegister);
				end, 
				GETGLOBAL = function(value, _)
					local register = bit32.band(bit32.rshift(value, 8), 255);
					local globalIndex = bit32.band(bit32.rshift(value, 24), 255);
					local globalConstant = code[instructionCounter];
					if instructionCounter == codeLength then
						error("Corrupted aux");
					end;
					instructionCounter = instructionCounter + 1;
					local constantIndex = globalConstant;
					globalConstant = constants[constantIndex];
					if globalConstant.type == 3 and isValidIdentifier(globalConstant.value) and string.sub(globalConstant.value, 1, 3) ~= "var" then
						debugPrintInstruction("GETGLOBAL " .. register .. ", " .. globalIndex .. " [" .. constantIndex .. "]", "var" .. register .. " = " .. globalConstant.value);
						return;
					else
						debugPrintInstruction("GETGLOBAL " .. register .. ", " .. globalIndex .. " [" .. constantIndex .. "]", "var" .. register .. " = getfenv()[" .. constantToString(globalConstant) .. "]");
						return;
					end;
				end, 
				SETGLOBAL = function(value, _)
					local register = bit32.band(bit32.rshift(value, 8), 255);
					local globalIndex = bit32.band(bit32.rshift(value, 24), 255);
					local globalConstant = code[instructionCounter];
					if instructionCounter == codeLength then
						error("Corrupted aux");
					end;
					instructionCounter = instructionCounter + 1;
					local constantIndex = globalConstant;
					globalConstant = constants[constantIndex];
					if globalConstant.type == 3 and isValidIdentifier(globalConstant.value) and string.sub(globalConstant.value, 1, 3) ~= "var" then
						debugPrintInstruction("SETGLOBAL " .. register .. ", " .. globalIndex .. " [" .. constantIndex .. "]", globalConstant.value .. " = var" .. register);
						return;
					else
						debugPrintInstruction("SETGLOBAL " .. register .. ", " .. globalIndex .. " [" .. constantIndex .. "]", "getfenv()[" .. constantToString(globalConstant) .. "] = var" .. register);
						return;
					end;
				end, 
				GETUPVAL = function(value, _)
					local register = bit32.band(bit32.rshift(value, 8), 255);
					local upvalueIndex16_23 = bit32.band(bit32.rshift(value, 16), 255);
					debugPrintInstruction("GETUPVAL " .. register .. ", " .. upvalueIndex16_23, "var" .. register .. " = " .. "up" .. upvalueIndex16_23);
				end, 
				SETUPVAL = function(value, _)
					local upvalueIndex8_15 = bit32.band(bit32.rshift(value, 8), 255);
					local upvalueIndex16_23_2 = bit32.band(bit32.rshift(value, 16), 255);
					debugPrintInstruction("SETUPVAL " .. upvalueIndex8_15 .. ", " .. upvalueIndex16_23_2, "up" .. upvalueIndex16_23_2 .. " = var" .. upvalueIndex8_15);
				end, 
				CLOSEUPVALS = function(unknownValue2, _)
					local upvalueIndex = bit32.band(bit32.rshift(unknownValue2, 8), 255);
					debugPrintInstruction("CLOSEUPVALS " .. upvalueIndex, "move_upvalues_to_heap(var" .. upvalueIndex .. "->...)");
				end, 
				GETIMPORT = function(value, _)
					local importIndex8_15 = bit32.band(bit32.rshift(value, 8), 255);
					local importIndex16_31 = bit32.band(bit32.rshift(value, 16), 65535);
					local codeInstruction = code[instructionCounter];
					if instructionCounter == codeLength then
						error("Corrupted aux");
					end;
					instructionCounter = instructionCounter + 1;
					local originalCodeInstruction = codeInstruction;
					codeInstruction = bit32.rrotate(bit32.band(originalCodeInstruction, 3221225472), 30);
					local constantValue1 = constants[bit32.rrotate(bit32.band(originalCodeInstruction, 1072693248), 20)];
					if codeInstruction == 2 then
						local constantValue2 = constants[bit32.rrotate(bit32.band(originalCodeInstruction, 1047552), 10)];
						local importedValue1 = nil;
						importedValue1 = if constantValue1.type == 3 and isValidIdentifier(constantValue1.value) and string.sub(constantValue1.value, 1, 3) ~= "var" then constantValue1.value else "getfenv()[" .. constantToString(constantValue1) .. "]";
						importedValue1 = if constantValue2.type == 3 and isValidIdentifier(constantValue2.value) and string.sub(constantValue2.value, 1, 3) ~= "var" then importedValue1 .. "." .. constantValue2.value else importedValue1 .. "[" .. constantToString(constantValue2) .. "]";
						debugPrintInstruction("GETIMPORT " .. importIndex8_15 .. ", " .. importIndex16_31 .. " [0x" .. (hexLookupTable[bit32.rshift(originalCodeInstruction, 24)] .. hexLookupTable[bit32.band(bit32.rrotate(originalCodeInstruction, 16), 255)] .. hexLookupTable[bit32.band(bit32.rrotate(originalCodeInstruction, 8), 255)] .. hexLookupTable[bit32.band(originalCodeInstruction, 255)]) .. "]", "var" .. importIndex8_15 .. " = " .. importedValue1);
						return;
					elseif codeInstruction == 3 then
						local constantValue3 = constants[bit32.rrotate(bit32.band(originalCodeInstruction, 1047552), 10)];
						local constantValue4 = constants[bit32.band(originalCodeInstruction, 1023)];
						local importedValue2 = nil;
						importedValue2 = if constantValue1.type == 3 and isValidIdentifier(constantValue1.value) and string.sub(constantValue1.value, 1, 3) ~= "var" then constantValue1.value else "getfenv()[" .. constantToString(constantValue1) .. "]";
						importedValue2 = if constantValue3.type == 3 and isValidIdentifier(constantValue3.value) and string.sub(constantValue3.value, 1, 3) ~= "var" then importedValue2 .. "." .. constantValue3.value else importedValue2 .. "[" .. constantToString(constantValue3) .. "]";
						importedValue2 = if constantValue4.type == 3 and isValidIdentifier(constantValue4.value) and string.sub(constantValue4.value, 1, 3) ~= "var" then importedValue2 .. "." .. constantValue4.value else importedValue2 .. "[" .. constantToString(constantValue4) .. "]";
						debugPrintInstruction("GETIMPORT " .. importIndex8_15 .. ", " .. importIndex16_31 .. " [0x" .. (hexLookupTable[bit32.rshift(originalCodeInstruction, 24)] .. hexLookupTable[bit32.band(bit32.rrotate(originalCodeInstruction, 16), 255)] .. hexLookupTable[bit32.band(bit32.rrotate(originalCodeInstruction, 8), 255)] .. hexLookupTable[bit32.band(originalCodeInstruction, 255)]) .. "]", "var" .. importIndex8_15 .. " = " .. importedValue2);
						return;
					elseif constantValue1.type == 3 and isValidIdentifier(constantValue1.value) and string.sub(constantValue1.value, 1, 3) ~= "var" then
						debugPrintInstruction("GETIMPORT " .. importIndex8_15 .. ", " .. importIndex16_31 .. " [0x" .. (hexLookupTable[bit32.rshift(originalCodeInstruction, 24)] .. hexLookupTable[bit32.band(bit32.rrotate(originalCodeInstruction, 16), 255)] .. hexLookupTable[bit32.band(bit32.rrotate(originalCodeInstruction, 8), 255)] .. hexLookupTable[bit32.band(originalCodeInstruction, 255)]) .. "]", "var" .. importIndex8_15 .. " = " .. constantValue1.value);
						return;
					else
						debugPrintInstruction("GETIMPORT " .. importIndex8_15 .. ", " .. importIndex16_31 .. " [0x" .. (hexLookupTable[bit32.rshift(originalCodeInstruction, 24)] .. hexLookupTable[bit32.band(bit32.rrotate(originalCodeInstruction, 16), 255)] .. hexLookupTable[bit32.band(bit32.rrotate(originalCodeInstruction, 8), 255)] .. hexLookupTable[bit32.band(originalCodeInstruction, 255)]) .. "]", "var" .. importIndex8_15 .. " = getfenv()[" .. constantToString(constantValue1) .. "]");
						return;
					end;
				end, 
				GETTABLE = function(tableInfo, _)
					local tableIndex8_15 = bit32.band(bit32.rshift(tableInfo, 8), 255);
					local tableIndex16_23 = bit32.band(bit32.rshift(tableInfo, 16), 255);
					local index1 = bit32.band(bit32.rshift(tableInfo, 24), 255);
					debugPrintInstruction("GETTABLE " .. tableIndex8_15 .. ", " .. tableIndex16_23 .. ", " .. index1, "var" .. tableIndex8_15 .. " = var" .. tableIndex16_23 .. "[var" .. index1 .. "]");
				end, 
				SETTABLE = function(valueInfo, _)
					local valueIndex8_15 = bit32.band(bit32.rshift(valueInfo, 8), 255);
					local valueIndex16_23 = bit32.band(bit32.rshift(valueInfo, 16), 255);
					local index2 = bit32.band(bit32.rshift(valueInfo, 24), 255);
					debugPrintInstruction("SETTABLE " .. valueIndex8_15 .. ", " .. valueIndex16_23 .. ", " .. index2, "var" .. valueIndex16_23 .. "[var" .. index2 .. "] = var" .. valueIndex8_15);
				end, 
				GETTABLEKS = function(tableInfo2, _)
					local tableIndex8_15_2 = bit32.band(bit32.rshift(tableInfo2, 8), 255);
					local tableIndex16_23_2 = bit32.band(bit32.rshift(tableInfo2, 16), 255);
					local index3 = bit32.band(bit32.rshift(tableInfo2, 24), 255);
					local constantValue5 = code[instructionCounter];
					if instructionCounter == codeLength then
						error("Corrupted aux");
					end;
					instructionCounter = instructionCounter + 1;
					local index = constantValue5;
					constantValue5 = constants[index];
					if constantValue5.type == 3 and isValidIdentifier(constantValue5.value) then
						debugPrintInstruction("GETTABLEKS " .. tableIndex8_15_2 .. ", " .. tableIndex16_23_2 .. ", " .. index3 .. " [" .. index .. "]", "var" .. tableIndex8_15_2 .. " = var" .. tableIndex16_23_2 .. "." .. constantValue5.value);
						return;
					else
						debugPrintInstruction("GETTABLEKS " .. tableIndex8_15_2 .. ", " .. tableIndex16_23_2 .. ", " .. index3 .. " [" .. index .. "]", "var" .. tableIndex8_15_2 .. " = var" .. tableIndex16_23_2 .. "[" .. constantToString(constantValue5) .. "]");
						return;
					end;
				end, 
				SETTABLEKS = function(valueInfo2, _)
					local valueIndex8_15_2 = bit32.band(bit32.rshift(valueInfo2, 8), 255);
					local valueIndex16_23_2 = bit32.band(bit32.rshift(valueInfo2, 16), 255);
					local index4 = bit32.band(bit32.rshift(valueInfo2, 24), 255);
					local constantValue6 = code[instructionCounter];
					if instructionCounter == codeLength then
						error("Corrupted aux");
					end;
					instructionCounter = instructionCounter + 1;
					local index = constantValue6;
					constantValue6 = constants[index];
					if constantValue6.type == 3 and isValidIdentifier(constantValue6.value) then
						debugPrintInstruction("SETTABLEKS " .. valueIndex8_15_2 .. ", " .. valueIndex16_23_2 .. ", " .. index4 .. " [" .. index .. "]", "var" .. valueIndex16_23_2 .. "." .. constantValue6.value .. " = var" .. valueIndex8_15_2);
						return;
					else
						debugPrintInstruction("SETTABLEKS " .. valueIndex8_15_2 .. ", " .. valueIndex16_23_2 .. ", " .. index4 .. " [" .. index .. "]", "var" .. valueIndex16_23_2 .. "[" .. constantToString(constantValue6) .. "] = var" .. valueIndex8_15_2);
						return;
					end;
				end, 
				GETTABLEN = function(arrayInfo, _)
					local arrayIndex8_15 = bit32.band(bit32.rshift(arrayInfo, 8), 255);
					local arrayIndex16_23 = bit32.band(bit32.rshift(arrayInfo, 16), 255);
					local index5 = bit32.band(bit32.rshift(arrayInfo, 24), 255);
					debugPrintInstruction("GETTABLEN " .. arrayIndex8_15 .. ", " .. arrayIndex16_23 .. ", " .. index5, "var" .. arrayIndex8_15 .. " = var" .. arrayIndex16_23 .. "[" .. index5 .. "]");
				end, 
				SETTABLEN = function(bytecodeInstruction1, _)
					local tableIndex1 = bit32.band(bit32.rshift(bytecodeInstruction1, 8), 255);
					local tableIndex2 = bit32.band(bit32.rshift(bytecodeInstruction1, 16), 255);
					local index6 = bit32.band(bit32.rshift(bytecodeInstruction1, 24), 255);
					debugPrintInstruction("SETTABLEN " .. tableIndex1 .. ", " .. tableIndex2 .. ", " .. index6, "var" .. tableIndex2 .. "[" .. index6 .. "] = var" .. tableIndex1);
				end, 
				NAMECALL = function(bytecodeInstruction2, _)
					local namecallIndex1 = bit32.band(bit32.rshift(bytecodeInstruction2, 8), 255);
					local namecallIndex2 = bit32.band(bit32.rshift(bytecodeInstruction2, 16), 255);
					local index7 = bit32.band(bit32.rshift(bytecodeInstruction2, 24), 255);
					local constantValue = code[instructionCounter];
					if instructionCounter == codeLength then
						error("Corrupted aux");
					end;
					instructionCounter = instructionCounter + 1;
					local index = constantValue;
					constantValue = constants[index];
					if constantValue.type == 3 and isValidIdentifier(constantValue.value) then
						debugPrintInstruction("NAMECALL " .. namecallIndex1 .. ", " .. namecallIndex2 .. ", " .. index7 .. " [" .. index .. "]", "var" .. namecallIndex1 + 1 .. " = var" .. namecallIndex2 .. "; var" .. namecallIndex1 .. " = var" .. namecallIndex2 .. "." .. constantValue.value .. " -- Invokes __namecall");
						return;
					else
						debugPrintInstruction("NAMECALL " .. namecallIndex1 .. ", " .. namecallIndex2 .. ", " .. index7 .. " [" .. index .. "]", "var" .. namecallIndex1 + 1 .. " = var" .. namecallIndex2 .. "; var" .. namecallIndex1 .. " = var" .. namecallIndex2 .. "[" .. constantToString(constantValue) .. "] -- Invokes __namecall");
						return;
					end;
				end, 
				CALL = function(bytecodeInstruction3, _)
					local callIndex1 = bit32.band(bit32.rshift(bytecodeInstruction3, 8), 255);
					local argumentCount = bit32.band(bit32.rshift(bytecodeInstruction3, 16), 255);
					local argumentCount = bit32.band(bit32.rshift(bytecodeInstruction3, 24), 255);
					local argumentCountMinusOne = argumentCount - 1;
					local returnCountMinusOne = argumentCount - 1;
					local argumentList = nil;
					if argumentCountMinusOne == -1 then
						argumentList = {
							"var" .. callIndex1 + 1 .. "->(top)"
						};
					else
						argumentList = {};
						for argumentIndex = callIndex1 + 1, callIndex1 + argumentCountMinusOne do
							table.insert(argumentList, "var" .. argumentIndex);
						end;
					end;
					if returnCountMinusOne == 0 then
						debugPrintInstruction("CALL " .. callIndex1 .. ", " .. argumentCount .. ", " .. argumentCount, "var" .. callIndex1 .. "(" .. table.concat(argumentList, ", ") .. ")");
						return;
					else
						local returnValues = nil;
						if returnCountMinusOne == -1 then
							returnValues = {
								"var" .. callIndex1 .. "->(top)"
							};
						else
							returnValues = {};
							for returnIndex = callIndex1, callIndex1 + returnCountMinusOne - 1 do
								table.insert(returnValues, "var" .. returnIndex);
							end;
						end;
						debugPrintInstruction("CALL " .. callIndex1 .. ", " .. argumentCount .. ", " .. argumentCount, table.concat(returnValues, ", ") .. " = var" .. callIndex1 .. "(" .. table.concat(argumentList, ", ") .. ")");
						return;
					end;
				end, 
				RETURN = function(unknownValue, _)
					local returnIndex = bit32.band(bit32.rshift(unknownValue, 8), 255);
					local returnCount = bit32.band(bit32.rshift(unknownValue, 16), 255);
					local returnCountAdjusted = returnCount - 1;
					if returnCountAdjusted == 0 then
						debugPrintInstruction("RETURN " .. returnIndex .. ", " .. returnCount, "return");
						return;
					elseif returnCountAdjusted == -1 then
						debugPrintInstruction("RETURN " .. returnIndex .. ", " .. returnCount, "return var" .. returnIndex .. "->(top)");
						return;
					else
						debugPrintInstruction("RETURN " .. returnIndex .. ", " .. returnCount, "return var" .. returnIndex .. "->var" .. returnIndex + returnCountAdjusted - 1);
						return;
					end;
				end, 
				JUMP = function(unknownValue3, _)
					buffer.writeu16(buffer8, 0, (bit32.band(bit32.rshift(unknownValue3, 16), 65535)));
					local jumpOffset = buffer.readi16(buffer8, 0);
					local jumpTarget = instructionCounter + jumpOffset;
					local jumpTargetList = jumpTable[jumpTarget];
					if jumpTargetList then
						table.insert(jumpTargetList, instructionCounter - 1);
					else
						jumpTable[jumpTarget] = {
							instructionCounter - 1
						};
					end;
					debugPrintInstruction("JUMP " .. jumpOffset, "-- goto [" .. jumpTarget .. "]");
				end, 
				JUMPBACK = function(unknownValue4, _)
					buffer.writeu16(buffer8, 0, (bit32.band(bit32.rshift(unknownValue4, 16), 65535)));
					local jumpBackOffset = buffer.readi16(buffer8, 0);
					local jumpBackTarget = instructionCounter + jumpBackOffset;
					local jumpBackTargetList = jumpTable[jumpBackTarget];
					if jumpBackTargetList then
						table.insert(jumpBackTargetList, instructionCounter - 1);
					else
						jumpTable[jumpBackTarget] = {
							instructionCounter - 1
						};
					end;
					debugPrintInstruction("JUMPBACK " .. jumpBackOffset, "-- goto [" .. jumpBackTarget .. "] (likely while/repeat loop)");
				end, 
				JUMPIF = function(unknownValue, _)
					local jumpIfIndex = bit32.band(bit32.rshift(unknownValue, 8), 255);
					local jumpIfOffset = bit32.band(bit32.rshift(unknownValue, 16), 65535);
					local jumpIfTarget = instructionCounter + jumpIfOffset;
					local jumpIfTargetList = jumpTable[jumpIfTarget];
					if jumpIfTargetList then
						table.insert(jumpIfTargetList, instructionCounter - 1);
					else
						jumpTable[jumpIfTarget] = {
							instructionCounter - 1
						};
					end;
					debugPrintInstruction("JUMPIF " .. jumpIfIndex .. ", " .. jumpIfOffset, "if var" .. jumpIfIndex .. " then goto [" .. jumpIfTarget .. "] end");
				end, 
				JUMPIFNOT = function(unknownValue, _)
					local jumpIfNotIndex = bit32.band(bit32.rshift(unknownValue, 8), 255);
					local jumpIfNotOffset = bit32.band(bit32.rshift(unknownValue, 16), 65535);
					local jumpIfNotTarget = instructionCounter + jumpIfNotOffset;
					local indexList = jumpTable[jumpIfNotTarget];
					if indexList then
						table.insert(indexList, instructionCounter - 1);
					else
						jumpTable[jumpIfNotTarget] = {
							instructionCounter - 1
						};
					end;
					debugPrintInstruction("JUMPIFNOT " .. jumpIfNotIndex .. ", " .. jumpIfNotOffset, "if not var" .. jumpIfNotIndex .. " then goto [" .. jumpIfNotTarget .. "] end");
				end, 
				JUMPIFEQ = function(unknownValue, _)
					local jumpIfEqualVariable = bit32.band(bit32.rshift(unknownValue, 8), 255);
					local jumpIfEqualOffset = bit32.band(bit32.rshift(unknownValue, 16), 65535);
					local jumpIfEqualTarget = code[instructionCounter];
					if instructionCounter == codeLength then
						error("Corrupted aux");
					end;
					instructionCounter = instructionCounter + 1;
					local comparisonValue1 = jumpIfEqualTarget;
					jumpIfEqualTarget = instructionCounter - 1 + jumpIfEqualOffset;
					local jumpIfEqualTargetList = jumpTable[jumpIfEqualTarget];
					if jumpIfEqualTargetList then
						table.insert(jumpIfEqualTargetList, instructionCounter - 2);
					else
						jumpTable[jumpIfEqualTarget] = {
							instructionCounter - 2
						};
					end;
					debugPrintInstruction("JUMPIFEQ " .. jumpIfEqualVariable .. ", " .. jumpIfEqualOffset .. " [" .. comparisonValue1 .. "]", "if var" .. jumpIfEqualVariable .. " == var" .. comparisonValue1 .. " then goto [" .. jumpIfEqualTarget .. "] end");
				end, 
				JUMPIFLE = function(unknownValue, _)
					local jumpIfLessEqualVariable = bit32.band(bit32.rshift(unknownValue, 8), 255);
					local jumpIfLessEqualOffset = bit32.band(bit32.rshift(unknownValue, 16), 65535);
					local jumpIfLessEqualTarget = code[instructionCounter];
					if instructionCounter == codeLength then
						error("Corrupted aux");
					end;
					instructionCounter = instructionCounter + 1;
					local comparisonValue2 = jumpIfLessEqualTarget;
					jumpIfLessEqualTarget = instructionCounter - 1 + jumpIfLessEqualOffset;
					local jumpIfLessEqualTargetList = jumpTable[jumpIfLessEqualTarget];
					if jumpIfLessEqualTargetList then
						table.insert(jumpIfLessEqualTargetList, instructionCounter - 2);
					else
						jumpTable[jumpIfLessEqualTarget] = {
							instructionCounter - 2
						};
					end;
					debugPrintInstruction("JUMPIFLE " .. jumpIfLessEqualVariable .. ", " .. jumpIfLessEqualOffset .. " [" .. comparisonValue2 .. "]", "if var" .. jumpIfLessEqualVariable .. " <= var" .. comparisonValue2 .. " then goto [" .. jumpIfLessEqualTarget .. "] end");
				end, 
				JUMPIFLT = function(unknownValue, _)
					local jumpIfLessThanVariable = bit32.band(bit32.rshift(unknownValue, 8), 255);
					local jumpIfLessThanOffset = bit32.band(bit32.rshift(unknownValue, 16), 65535);
					local jumpIfLessThanTarget = code[instructionCounter];
					if instructionCounter == codeLength then
						error("Corrupted aux");
					end;
					instructionCounter = instructionCounter + 1;
					local comparisonValue3 = jumpIfLessThanTarget;
					jumpIfLessThanTarget = instructionCounter - 1 + jumpIfLessThanOffset;
					local jumpIfLessThanTargetList = jumpTable[jumpIfLessThanTarget];
					if jumpIfLessThanTargetList then
						table.insert(jumpIfLessThanTargetList, instructionCounter - 2);
					else
						jumpTable[jumpIfLessThanTarget] = {
							instructionCounter - 2
						};
					end;
					debugPrintInstruction("JUMPIFLT " .. jumpIfLessThanVariable .. ", " .. jumpIfLessThanOffset .. " [" .. comparisonValue3 .. "]", "if var" .. jumpIfLessThanVariable .. " < var" .. comparisonValue3 .. " then goto [" .. jumpIfLessThanTarget .. "] end");
				end, 
				JUMPIFNOTEQ = function(unknownValue, _)
					local jumpIfNotEqualVariable = bit32.band(bit32.rshift(unknownValue, 8), 255);
					local jumpIfNotEqualOffset = bit32.band(bit32.rshift(unknownValue, 16), 65535);
					local jumpIfNotEqualTarget = code[instructionCounter];
					if instructionCounter == codeLength then
						error("Corrupted aux");
					end;
					instructionCounter = instructionCounter + 1;
					local comparisonValue4 = jumpIfNotEqualTarget;
					jumpIfNotEqualTarget = instructionCounter - 1 + jumpIfNotEqualOffset;
					local jumpIfNotEqualTargetList = jumpTable[jumpIfNotEqualTarget];
					if jumpIfNotEqualTargetList then
						table.insert(jumpIfNotEqualTargetList, instructionCounter - 2);
					else
						jumpTable[jumpIfNotEqualTarget] = {
							instructionCounter - 2
						};
					end;
					debugPrintInstruction("JUMPIFNOTEQ " .. jumpIfNotEqualVariable .. ", " .. jumpIfNotEqualOffset .. " [" .. comparisonValue4 .. "]", "if var" .. jumpIfNotEqualVariable .. " ~= var" .. comparisonValue4 .. " then goto [" .. jumpIfNotEqualTarget .. "] end");
				end, 
				JUMPIFNOTLE = function(unknownValue, _)
					local jumpIfNotLessEqualVariable = bit32.band(bit32.rshift(unknownValue, 8), 255);
					local jumpIfNotLessEqualOffset = bit32.band(bit32.rshift(unknownValue, 16), 65535);
					local jumpIfNotLessEqualTarget = code[instructionCounter];
					if instructionCounter == codeLength then
						error("Corrupted aux");
					end;
					instructionCounter = instructionCounter + 1;
					local comparisonValue5 = jumpIfNotLessEqualTarget;
					jumpIfNotLessEqualTarget = instructionCounter - 1 + jumpIfNotLessEqualOffset;
					local jumpIfNotLessEqualTargetList = jumpTable[jumpIfNotLessEqualTarget];
					if jumpIfNotLessEqualTargetList then
						table.insert(jumpIfNotLessEqualTargetList, instructionCounter - 2);
					else
						jumpTable[jumpIfNotLessEqualTarget] = {
							instructionCounter - 2
						};
					end;
					debugPrintInstruction("JUMPIFNOTLE " .. jumpIfNotLessEqualVariable .. ", " .. jumpIfNotLessEqualOffset .. " [" .. comparisonValue5 .. "]", "if var" .. jumpIfNotLessEqualVariable .. " > var" .. comparisonValue5 .. " then goto [" .. jumpIfNotLessEqualTarget .. "] end");
				end, 
				JUMPIFNOTLT = function(unknownValue, _)
					local jumpIfNotLessThanVariable = bit32.band(bit32.rshift(unknownValue, 8), 255);
					local jumpIfNotLessThanOffset = bit32.band(bit32.rshift(unknownValue, 16), 65535);
					local jumpIfNotLessThanTarget = code[instructionCounter];
					if instructionCounter == codeLength then
						error("Corrupted aux");
					end;
					instructionCounter = instructionCounter + 1;
					local comparisonVariable = jumpIfNotLessThanTarget;
					jumpIfNotLessThanTarget = instructionCounter - 1 + jumpIfNotLessThanOffset;
					local jumpIfNotLessThanTargetList = jumpTable[jumpIfNotLessThanTarget];
					if jumpIfNotLessThanTargetList then
						table.insert(jumpIfNotLessThanTargetList, instructionCounter - 2);
					else
						jumpTable[jumpIfNotLessThanTarget] = {
							instructionCounter - 2
						};
					end;
					debugPrintInstruction("JUMPIFNOTLT " .. jumpIfNotLessThanVariable .. ", " .. jumpIfNotLessThanOffset .. " [" .. comparisonVariable .. "]", "if var" .. jumpIfNotLessThanVariable .. " >= var" .. comparisonVariable .. " then goto [" .. jumpIfNotLessThanTarget .. "] end");
				end, 
				ADD = function(addInstruction, _)
					local addVariable = bit32.band(bit32.rshift(addInstruction, 8), 255);
					local addOperand1 = bit32.band(bit32.rshift(addInstruction, 16), 255);
					local bandedValue1 = bit32.band(bit32.rshift(addInstruction, 24), 255);
					if addVariable == addOperand1 then
						debugPrintInstruction("ADD " .. addVariable .. ", " .. addOperand1 .. ", " .. bandedValue1, "var" .. addVariable .. " += var" .. bandedValue1);
						return;
					elseif addVariable == bandedValue1 then
						debugPrintInstruction("ADD " .. addVariable .. ", " .. addOperand1 .. ", " .. bandedValue1, "var" .. addVariable .. " += var" .. addOperand1);
						return;
					else
						debugPrintInstruction("ADD " .. addVariable .. ", " .. addOperand1 .. ", " .. bandedValue1, "var" .. addVariable .. " = var" .. addOperand1 .. " + var" .. bandedValue1);
						return;
					end;
				end, 
				SUB = function(subtractInstruction, _)
					local subtractVariable = bit32.band(bit32.rshift(subtractInstruction, 8), 255);
					local byte2 = bit32.band(bit32.rshift(subtractInstruction, 16), 255);
					local bandedValue2 = bit32.band(bit32.rshift(subtractInstruction, 24), 255);
					if subtractVariable == byte2 then
						debugPrintInstruction("SUB " .. subtractVariable .. ", " .. byte2 .. ", " .. bandedValue2, "var" .. subtractVariable .. " -= var" .. bandedValue2);
						return;
					else
						debugPrintInstruction("SUB " .. subtractVariable .. ", " .. byte2 .. ", " .. bandedValue2, "var" .. subtractVariable .. " = var" .. byte2 .. " - var" .. bandedValue2);
						return;
					end;
				end, 
				MUL = function(colorValue, _)
					local byte1 = bit32.band(bit32.rshift(colorValue, 8), 255);
					local byte2 = bit32.band(bit32.rshift(colorValue, 16), 255);
					local bandedValue3 = bit32.band(bit32.rshift(colorValue, 24), 255);
					if byte1 == byte2 then
						debugPrintInstruction("MUL " .. byte1 .. ", " .. byte2 .. ", " .. bandedValue3, "var" .. byte1 .. " *= var" .. bandedValue3);
						return;
					elseif byte1 == bandedValue3 then
						debugPrintInstruction("MUL " .. byte1 .. ", " .. byte2 .. ", " .. bandedValue3, "var" .. byte1 .. " *= var" .. byte2);
						return;
					else
						debugPrintInstruction("MUL " .. byte1 .. ", " .. byte2 .. ", " .. bandedValue3, "var" .. byte1 .. " = var" .. byte2 .. " * var" .. bandedValue3);
						return;
					end;
				end, 
				DIV = function(colorValue, _)
					local byte1 = bit32.band(bit32.rshift(colorValue, 8), 255);
					local byte2 = bit32.band(bit32.rshift(colorValue, 16), 255);
					local bandedValue4 = bit32.band(bit32.rshift(colorValue, 24), 255);
					if byte1 == byte2 then
						debugPrintInstruction("DIV " .. byte1 .. ", " .. byte2 .. ", " .. bandedValue4, "var" .. byte1 .. " /= var" .. bandedValue4);
						return;
					else
						debugPrintInstruction("DIV " .. byte1 .. ", " .. byte2 .. ", " .. bandedValue4, "var" .. byte1 .. " = var" .. byte2 .. " / var" .. bandedValue4);
						return;
					end;
				end, 
				MOD = function(colorValue, _)
					local byte1 = bit32.band(bit32.rshift(colorValue, 8), 255);
					local byte2 = bit32.band(bit32.rshift(colorValue, 16), 255);
					local bandedValue5 = bit32.band(bit32.rshift(colorValue, 24), 255);
					if byte1 == byte2 then
						debugPrintInstruction("MOD " .. byte1 .. ", " .. byte2 .. ", " .. bandedValue5, "var" .. byte1 .. " %= var" .. bandedValue5);
						return;
					else
						debugPrintInstruction("MOD " .. byte1 .. ", " .. byte2 .. ", " .. bandedValue5, "var" .. byte1 .. " = var" .. byte2 .. " % var" .. bandedValue5);
						return;
					end;
				end, 
				POW = function(colorValue, _)
					local byte1 = bit32.band(bit32.rshift(colorValue, 8), 255);
					local byte2 = bit32.band(bit32.rshift(colorValue, 16), 255);
					local bandedValue6 = bit32.band(bit32.rshift(colorValue, 24), 255);
					if byte1 == byte2 then
						debugPrintInstruction("POW " .. byte1 .. ", " .. byte2 .. ", " .. bandedValue6, "var" .. byte1 .. " ^= var" .. bandedValue6);
						return;
					else
						debugPrintInstruction("POW " .. byte1 .. ", " .. byte2 .. ", " .. bandedValue6, "var" .. byte1 .. " = var" .. byte2 .. " ^ var" .. bandedValue6);
						return;
					end;
				end, 
				ADDK = function(colorValue, _)
					local byte1 = bit32.band(bit32.rshift(colorValue, 8), 255);
					local byte2 = bit32.band(bit32.rshift(colorValue, 16), 255);
					local constantIndex = bit32.band(bit32.rshift(colorValue, 24), 255);
					local constantValue = constants[constantIndex];
					if byte1 == byte2 then
						debugPrintInstruction("ADDK " .. byte1 .. ", " .. byte2 .. ", " .. constantIndex, "var" .. byte1 .. " += " .. constantToString(constantValue));
						return;
					else
						debugPrintInstruction("ADDK " .. byte1 .. ", " .. byte2 .. ", " .. constantIndex, "var" .. byte1 .. " = var" .. byte2 .. " + " .. constantToString(constantValue));
						return;
					end;
				end, 
				SUBK = function(colorValue, _)
					local byte1 = bit32.band(bit32.rshift(colorValue, 8), 255);
					local byte2 = bit32.band(bit32.rshift(colorValue, 16), 255);
					local constantIndex = bit32.band(bit32.rshift(colorValue, 24), 255);
					local constantValue = constants[constantIndex];
					if byte1 == byte2 then
						debugPrintInstruction("SUBK " .. byte1 .. ", " .. byte2 .. ", " .. constantIndex, "var" .. byte1 .. " -= " .. constantToString(constantValue));
						return;
					else
						debugPrintInstruction("SUBK " .. byte1 .. ", " .. byte2 .. ", " .. constantIndex, "var" .. byte1 .. " = var" .. byte2 .. " - " .. constantToString(constantValue));
						return;
					end;
				end, 
				MULK = function(colorValue, _)
					local byte1 = bit32.band(bit32.rshift(colorValue, 8), 255);
					local byte2 = bit32.band(bit32.rshift(colorValue, 16), 255);
					local constantIndex = bit32.band(bit32.rshift(colorValue, 24), 255);
					local constantValue = constants[constantIndex];
					if byte1 == byte2 then
						debugPrintInstruction("MULK " .. byte1 .. ", " .. byte2 .. ", " .. constantIndex, "var" .. byte1 .. " *= " .. constantToString(constantValue));
						return;
					else
						debugPrintInstruction("MULK " .. byte1 .. ", " .. byte2 .. ", " .. constantIndex, "var" .. byte1 .. " = var" .. byte2 .. " * " .. constantToString(constantValue));
						return;
					end;
				end, 
				DIVK = function(colorValue, _)
					local byte1 = bit32.band(bit32.rshift(colorValue, 8), 255);
					local byte2 = bit32.band(bit32.rshift(colorValue, 16), 255);
					local constantIndex = bit32.band(bit32.rshift(colorValue, 24), 255);
					local constantValue = constants[constantIndex];
					if byte1 == byte2 then
						debugPrintInstruction("DIVK " .. byte1 .. ", " .. byte2 .. ", " .. constantIndex, "var" .. byte1 .. " /= " .. constantToString(constantValue));
						return;
					else
						debugPrintInstruction("DIVK " .. byte1 .. ", " .. byte2 .. ", " .. constantIndex, "var" .. byte1 .. " = var" .. byte2 .. " / " .. constantToString(constantValue));
						return;
					end;
				end, 
				MODK = function(integerValue, _)
					local byte1 = bit32.band(bit32.rshift(integerValue, 8), 255);
					local byte2 = bit32.band(bit32.rshift(integerValue, 16), 255);
					local byte3 = bit32.band(bit32.rshift(integerValue, 24), 255);
					local constantValue = constants[byte3];
					if byte1 == byte2 then
						debugPrintInstruction("MODK " .. byte1 .. ", " .. byte2 .. ", " .. byte3, "var" .. byte1 .. " %= " .. constantToString(constantValue));
						return;
					else
						debugPrintInstruction("MODK " .. byte1 .. ", " .. byte2 .. ", " .. byte3, "var" .. byte1 .. " = var" .. byte2 .. " % " .. constantToString(constantValue));
						return;
					end;
				end, 
				POWK = function(integerValue2, _)
					local byte1 = bit32.band(bit32.rshift(integerValue2, 8), 255);
					local byte2_2 = bit32.band(bit32.rshift(integerValue2, 16), 255);
					local byte3_2 = bit32.band(bit32.rshift(integerValue2, 24), 255);
					local constantValue = constants[byte3_2];
					if byte1 == byte2_2 then
						debugPrintInstruction("POWK " .. byte1 .. ", " .. byte2_2 .. ", " .. byte3_2, "var" .. byte1 .. " ^= " .. constantToString(constantValue));
						return;
					else
						debugPrintInstruction("POWK " .. byte1 .. ", " .. byte2_2 .. ", " .. byte3_2, "var" .. byte1 .. " = var" .. byte2_2 .. " ^ " .. constantToString(constantValue));
						return;
					end;
				end, 
				AND = function(integerValue3, _)
					local byte1_3 = bit32.band(bit32.rshift(integerValue3, 8), 255);
					local byte2_3 = bit32.band(bit32.rshift(integerValue3, 16), 255);
					local bandedValue7 = bit32.band(bit32.rshift(integerValue3, 24), 255);
					debugPrintInstruction("AND " .. byte1_3 .. ", " .. byte2_3 .. ", " .. bandedValue7, "var" .. byte1_3 .. " = var" .. byte2_3 .. " and var" .. bandedValue7);
				end, 
				OR = function(integerValue4, _)
					local byte1_4 = bit32.band(bit32.rshift(integerValue4, 8), 255);
					local byte2_4 = bit32.band(bit32.rshift(integerValue4, 16), 255);
					local bandedValue8 = bit32.band(bit32.rshift(integerValue4, 24), 255);
					debugPrintInstruction("OR " .. byte1_4 .. ", " .. byte2_4 .. ", " .. bandedValue8, "var" .. byte1_4 .. " = var" .. byte2_4 .. " or var" .. bandedValue8);
				end, 
				ANDK = function(integerValue5, _)
					local byte1_5 = bit32.band(bit32.rshift(integerValue5, 8), 255);
					local byte2_5 = bit32.band(bit32.rshift(integerValue5, 16), 255);
					local byte3_5 = bit32.band(bit32.rshift(integerValue5, 24), 255);
					local constantValue = constants[byte3_5];
					debugPrintInstruction("ANDK " .. byte1_5 .. ", " .. byte2_5 .. ", " .. byte3_5, "var" .. byte1_5 .. " = var" .. byte2_5 .. " and " .. constantToString(constantValue));
				end, 
				ORK = function(integerValue6, _)
					local byte1_6 = bit32.band(bit32.rshift(integerValue6, 8), 255);
					local byte2_6 = bit32.band(bit32.rshift(integerValue6, 16), 255);
					local byte3_6 = bit32.band(bit32.rshift(integerValue6, 24), 255);
					local constantValue = constants[byte3_6];
					debugPrintInstruction("ORK " .. byte1_6 .. ", " .. byte2_6 .. ", " .. byte3_6, "var" .. byte1_6 .. " = var" .. byte2_6 .. " or " .. constantToString(constantValue));
				end, 
				CONCAT = function(integerValue7, _)
					local byte1_7 = bit32.band(bit32.rshift(integerValue7, 8), 255);
					local byte2_7 = bit32.band(bit32.rshift(integerValue7, 16), 255);
					local byte3_7 = bit32.band(bit32.rshift(integerValue7, 24), 255);
					local stringPartsList = {};
					for index = byte2_7, byte3_7 do
						table.insert(stringPartsList, "var" .. index);
					end;
					if byte1_7 == byte2_7 and byte2_7 <= byte3_7 then
						table.remove(stringPartsList, 1);
						debugPrintInstruction("CONCAT " .. byte1_7 .. ", " .. byte2_7 .. ", " .. byte3_7, "var" .. byte1_7 .. " ..= " .. table.concat(stringPartsList, ".."));
						return;
					else
						debugPrintInstruction("CONCAT " .. byte1_7 .. ", " .. byte2_7 .. ", " .. byte3_7, "var" .. byte1_7 .. " = " .. table.concat(stringPartsList, ".."));
						return;
					end;
				end, 
				NOT = function(unknownValue, _)
					local byte1_8 = bit32.band(bit32.rshift(unknownValue, 8), 255);
					local byte2_8 = bit32.band(bit32.rshift(unknownValue, 16), 255);
					debugPrintInstruction("NOT " .. byte1_8 .. ", " .. byte2_8, "var" .. byte1_8 .. " = not var" .. byte2_8);
				end, 
				MINUS = function(unknownValue, _)
					local byte1_9 = bit32.band(bit32.rshift(unknownValue, 8), 255);
					local byte2_9 = bit32.band(bit32.rshift(unknownValue, 16), 255);
					debugPrintInstruction("MINUS " .. byte1_9 .. ", " .. byte2_9, "var" .. byte1_9 .. " = -var" .. byte2_9);
				end, 
				LENGTH = function(unknownValue, _)
					local lengthByte1 = bit32.band(bit32.rshift(unknownValue, 8), 255);
					local lengthByte2 = bit32.band(bit32.rshift(unknownValue, 16), 255);
					debugPrintInstruction("LENGTH " .. lengthByte1 .. ", " .. lengthByte2, "var" .. lengthByte1 .. " = #var" .. lengthByte2);
				end, 
				NEWTABLE = function(unknownValue, _)
					local newTableByte1 = bit32.band(bit32.rshift(unknownValue, 8), 255);
					local newTableByte2 = bit32.band(bit32.rshift(unknownValue, 16), 255);
					local _ = newTableByte2 == 0 and 0 or bit32.lrotate(1, newTableByte2 - 1);
					local codeValue1 = code[instructionCounter];
					if instructionCounter == codeLength then
						error("Corrupted aux");
					end;
					instructionCounter = instructionCounter + 1;
					local localCodeValue1 = codeValue1;
					if localCodeValue1 > 0 then
						debugPrintInstruction("NEWTABLE " .. newTableByte1 .. ", " .. newTableByte2 .. " [" .. localCodeValue1 .. "]", "var" .. newTableByte1 .. " = table.create(" .. localCodeValue1 .. ")");
						return;
					else
						debugPrintInstruction("NEWTABLE " .. newTableByte1 .. ", " .. newTableByte2 .. " [" .. localCodeValue1 .. "]", "var" .. newTableByte1 .. " = {}");
						return;
					end;
				end, 
				DUPTABLE = function(unknownValue, _)
					local tableIndex = bit32.band(bit32.rshift(unknownValue, 8), 255);
					debugPrintInstruction("DUPTABLE " .. tableIndex .. ", " .. bit32.band(bit32.rshift(unknownValue, 16), 65535), "var" .. tableIndex .. " = {}");
				end, 
				SETLIST = function(setListSourceValue, _)
					local setListByte1 = bit32.band(bit32.rshift(setListSourceValue, 8), 255);
					local setListByte2 = bit32.band(bit32.rshift(setListSourceValue, 16), 255);
					local listCount = bit32.band(bit32.rshift(setListSourceValue, 24), 255);
					local offsetValue = listCount - 1;
					local codeValue2 = code[instructionCounter];
					if instructionCounter == codeLength then
						error("Corrupted aux");
					end;
					instructionCounter = instructionCounter + 1;
					local indexVariable = codeValue2;
					if offsetValue == -1 then
						debugPrintInstruction("SETLIST " .. setListByte1 .. ", " .. setListByte2 .. ", " .. listCount .. " [" .. indexVariable .. "]", "var" .. setListByte1 .. "[" .. indexVariable .. "->(top)] = var" .. setListByte2 .. "->(top)");
						return;
					else
						debugPrintInstruction("SETLIST " .. setListByte1 .. ", " .. setListByte2 .. ", " .. listCount .. " [" .. indexVariable .. "]", "var" .. setListByte1 .. "[" .. indexVariable .. "->" .. indexVariable + offsetValue - 1 .. "] = var" .. setListByte2 .. "->var" .. setListByte2 + offsetValue - 1);
						return;
					end;
				end, 
				FORNPREP = function(unknownValue, _)
					local forNPrepByte1 = bit32.band(bit32.rshift(unknownValue, 8), 255);
					buffer.writeu16(buffer8, 0, (bit32.band(bit32.rshift(unknownValue, 16), 65535)));
					local forNPrepOffset = buffer.readi16(buffer8, 0);
					local forNPrepTarget = instructionCounter + forNPrepOffset;
					local forNPrepValue = jumpTable[forNPrepTarget];
					if forNPrepValue then
						table.insert(forNPrepValue, instructionCounter - 1);
					else
						jumpTable[forNPrepTarget] = {
							instructionCounter - 1
						};
					end;
					debugPrintInstruction("FORNPREP " .. forNPrepByte1 .. ", " .. forNPrepOffset, "for var" .. forNPrepByte1 + 2 .. " = var" .. forNPrepByte1 + 2 .. ", var" .. forNPrepByte1 .. ", var" .. forNPrepByte1 + 1 .. " do -- If loop shouldn't start (var" .. forNPrepByte1 + 2 .. " > var" .. forNPrepByte1 .. ") then goto [" .. forNPrepTarget .. "]");
				end, 
				FORNLOOP = function(unknownValue, _)
					local forNLoopByte1 = bit32.band(bit32.rshift(unknownValue, 8), 255);
					buffer.writeu16(buffer8, 0, (bit32.band(bit32.rshift(unknownValue, 16), 65535)));
					local forNLoopOffset = buffer.readi16(buffer8, 0);
					local forNLoopTarget = instructionCounter + forNLoopOffset;
					local forNLoopValue = jumpTable[forNLoopTarget];
					if forNLoopValue then
						table.insert(forNLoopValue, instructionCounter - 1);
					else
						jumpTable[forNLoopTarget] = {
							instructionCounter - 1
						};
					end;
					debugPrintInstruction("FORNLOOP " .. forNLoopByte1 .. ", " .. forNLoopOffset, "var" .. forNLoopByte1 + 2 .. " += var" .. forNLoopByte1 + 1 .. "; if var" .. forNLoopByte1 + 2 .. " <= var" .. forNLoopByte1 .. " then goto [" .. forNLoopTarget .. "] end");
				end, 
				FORGLOOP = function(unknownValue, _)
					local forGLoopByte1 = bit32.band(bit32.rshift(unknownValue, 8), 255);
					buffer.writeu16(buffer8, 0, (bit32.band(bit32.rshift(unknownValue, 16), 65535)));
					local forGLoopOffset = buffer.readi16(buffer8, 0);
					local codeValue3 = code[instructionCounter];
					if instructionCounter == codeLength then
						error("Corrupted aux");
					end;
					instructionCounter = instructionCounter + 1;
					local localCodeValue3 = codeValue3;
					codeValue3 = bit32.band(localCodeValue3, 255);
					local variableList = table.create(codeValue3);
					for variableIndex = 1, codeValue3 do
						table.insert(variableList, "var" .. forGLoopByte1 + 2 + variableIndex);
					end;
					local forGLoopTarget = instructionCounter + forGLoopOffset - 1;
					local forGLoopValue = jumpTable[forGLoopTarget];
					if forGLoopValue then
						table.insert(forGLoopValue, instructionCounter - 2);
					else
						jumpTable[forGLoopTarget] = {
							instructionCounter - 2
						};
					end;
					if bit32.band(localCodeValue3, 2147483648) > 0 then
						debugPrintInstruction("FORGLOOP " .. forGLoopByte1 .. ", " .. forGLoopOffset .. " [0x" .. (hexLookupTable[bit32.rshift(localCodeValue3, 24)] .. hexLookupTable[bit32.band(bit32.rrotate(localCodeValue3, 16), 255)] .. hexLookupTable[bit32.band(bit32.rrotate(localCodeValue3, 8), 255)] .. hexLookupTable[bit32.band(localCodeValue3, 255)]) .. "]", table.concat(variableList, ", ") .. " = var" .. forGLoopByte1 .. "(var" .. forGLoopByte1 + 1 .. ", var" .. forGLoopByte1 + 2 .. "); if var" .. forGLoopByte1 + 3 .. " ~= nil then goto [" .. forGLoopTarget .. "]");
						return;
					else
						debugPrintInstruction("FORGLOOP " .. forGLoopByte1 .. ", " .. forGLoopOffset .. " [0x" .. (hexLookupTable[bit32.rshift(localCodeValue3, 24)] .. hexLookupTable[bit32.band(bit32.rrotate(localCodeValue3, 16), 255)] .. hexLookupTable[bit32.band(bit32.rrotate(localCodeValue3, 8), 255)] .. hexLookupTable[bit32.band(localCodeValue3, 255)]) .. "]", table.concat(variableList, ", ") .. " = var" .. forGLoopByte1 .. "(var" .. forGLoopByte1 + 1 .. ", var" .. forGLoopByte1 + 2 .. "); if var" .. forGLoopByte1 + 3 .. " ~= nil then goto [" .. forGLoopTarget .. "]");
						return;
					end;
				end, 
				FORGPREP = function(unknownValue, _)
					local forGPrepByte1 = bit32.band(bit32.rshift(unknownValue, 8), 255);
					buffer.writeu16(buffer8, 0, (bit32.band(bit32.rshift(unknownValue, 16), 65535)));
					local forGPrepOffset = buffer.readi16(buffer8, 0);
					local forGPrepTarget = instructionCounter + forGPrepOffset;
					local indexList = jumpTable[forGPrepTarget];
					if indexList then
						table.insert(indexList, instructionCounter - 1);
					else
						jumpTable[forGPrepTarget] = {
							instructionCounter - 1
						};
					end;
					debugPrintInstruction("FORGPREP " .. forGPrepByte1 .. ", " .. forGPrepOffset, "for var" .. forGPrepByte1 + 3 .. "->... in var" .. forGPrepByte1 .. ", var" .. forGPrepByte1 + 1 .. ", var" .. forGPrepByte1 + 2 .. " do -- If loop shouldn't start then goto [" .. forGPrepTarget .. "]");
				end, 
				FORGPREP_INEXT = function(unknownValue, _)
					local forLoopIndexByte = bit32.band(bit32.rshift(unknownValue, 8), 255);
					buffer.writeu16(buffer8, 0, (bit32.band(bit32.rshift(unknownValue, 16), 65535)));
					local forLoopOffset = buffer.readi16(buffer8, 0);
					local forLoopTargetAddress = instructionCounter + forLoopOffset;
					local targetIndexList = jumpTable[forLoopTargetAddress];
					if targetIndexList then
						table.insert(targetIndexList, instructionCounter - 1);
					else
						jumpTable[forLoopTargetAddress] = {
							instructionCounter - 1
						};
					end;
					debugPrintInstruction("FORGPREP_INEXT " .. forLoopIndexByte .. ", " .. forLoopOffset, "for var" .. forLoopIndexByte + 3 .. "->... in var" .. forLoopIndexByte .. ", var" .. forLoopIndexByte + 1 .. ", var" .. forLoopIndexByte + 2 .. " do -- If loop shouldn't start then goto [" .. forLoopTargetAddress .. "]");
				end, 
				DEP_FORGLOOP_INEXT = function(_, _)
					debugPrintInstruction("DEP_FORGLOOP_INEXT", "-- Deprecated instruction, send me some bytecode that has this and I'll support it");
				end, 
				FORGPREP_NEXT = function(unknownValue, _)
					local forLoopIndexByte2 = bit32.band(bit32.rshift(unknownValue, 8), 255);
					buffer.writeu16(buffer8, 0, (bit32.band(bit32.rshift(unknownValue, 16), 65535)));
					local forLoopOffset2 = buffer.readi16(buffer8, 0);
					local forLoopTargetAddress2 = instructionCounter + forLoopOffset2;
					local targetIndexList2 = jumpTable[forLoopTargetAddress2];
					if targetIndexList2 then
						table.insert(targetIndexList2, instructionCounter - 1);
					else
						jumpTable[forLoopTargetAddress2] = {
							instructionCounter - 1
						};
					end;
					debugPrintInstruction("FORGPREP_NEXT " .. forLoopIndexByte2 .. ", " .. forLoopOffset2, "for var" .. forLoopIndexByte2 + 3 .. "->... in var" .. forLoopIndexByte2 .. ", var" .. forLoopIndexByte2 + 1 .. ", var" .. forLoopIndexByte2 + 2 .. " do -- If loop shouldn't start then goto [" .. forLoopTargetAddress2 .. "]");
				end, 
				NATIVECALL = function(_, _)
					debugPrintInstruction("NATIVECALL", "-- Call to native code");
				end, 
				GETVARARGS = function(unknownValue, _)
					local varArgsCountByte = bit32.band(bit32.rshift(unknownValue, 8), 255);
					local varArgsTopByte = bit32.band(bit32.rshift(unknownValue, 16), 255);
					local varArgsCountMinusOne = varArgsTopByte - 1;
					if varArgsCountMinusOne == 0 then
						debugPrintInstruction("GETVARARGS " .. varArgsCountByte .. ", " .. varArgsTopByte, "var" .. varArgsCountByte .. " = ... -- No variables");
						return;
					elseif varArgsCountMinusOne == -1 then
						if varArgsCountByte == 1 then
							debugPrintInstruction("GETVARARGS " .. varArgsCountByte .. ", " .. varArgsTopByte, "var" .. varArgsCountByte .. "->(top) = ... -- Load (top) variables");
							return;
						else
							debugPrintInstruction("GETVARARGS " .. varArgsCountByte .. ", " .. varArgsTopByte, "var" .. varArgsCountByte .. "->(top) = ... -- Load (top) - " .. varArgsCountByte - 1 .. " variables");
							return;
						end;
					else
						debugPrintInstruction("GETVARARGS " .. varArgsCountByte .. ", " .. varArgsTopByte, "var" .. varArgsCountByte .. " = ... -- Load " .. varArgsCountMinusOne .. " variable" .. (varArgsCountMinusOne == 1 and "" or "s"));
						return;
					end;
				end, 
				NEWCLOSURE = function(unknownValue, unknownValue)
					local newClosureIndexByte = bit32.band(bit32.rshift(unknownValue, 8), 255);
					local prototypeIndex = bit32.band(bit32.rshift(unknownValue, 16), 65535);
					local prototype = protos[prototypeIndex];
					local _ = currentLine;
					local debugName = prototype.debug_name;
					if not debugName then
						local functionNamePrefix = "func";
						uniqueIdCounter = uniqueIdCounter + 1;
						debugName = functionNamePrefix .. uniqueIdCounter;
					end;
					prototype.debug_name = debugName;
					disassembleProto(prototype, unknownValue);
					table.insert(disassemblyLines, currentLine);
					currentLine = indentation .. "[" .. instructionCounter - 1 .. "] #" .. arrayIndex;
					debugPrintInstruction("NEWCLOSURE " .. newClosureIndexByte .. ", " .. prototypeIndex, "var" .. newClosureIndexByte .. " = " .. prototype.debug_name);
					debugName = 0;
					while true do
						local bytecodeInstruction = code[instructionCounter];
						if bytecodeInstruction and opcodeMapByCode[bit32.band(bytecodeInstruction, 255)].opname == "CAPTURE" then
							table.insert(disassemblyLines, currentLine);
							currentLine = indentation .. "[" .. instructionCounter - 1 .. "] #" .. arrayIndex;
							local captureType = bit32.band(bit32.rshift(bytecodeInstruction, 8), 255);
							local variableIndex = bit32.band(bit32.rshift(bytecodeInstruction, 16), 255);
							if captureType == 0 then
								debugPrintInstruction("CAPTURE " .. captureType .. ", " .. variableIndex, "up" .. debugName .. " = var" .. variableIndex .. " -- Readable");
							elseif captureType == 1 then
								debugPrintInstruction("CAPTURE " .. captureType .. ", " .. variableIndex, "up" .. debugName .. " = var" .. variableIndex .. " -- Readable and writable");
							elseif captureType == 2 then
								debugPrintInstruction("CAPTURE " .. captureType .. ", " .. variableIndex, "up" .. debugName .. " = up" .. variableIndex);
							else
								debugPrintInstruction("CAPTURE " .. captureType .. ", " .. variableIndex, "up" .. debugName .. " = ??? -- Invalid capture type");
							end;
							instructionCounter = instructionCounter + 1;
							debugName = debugName + 1;
						else
							break;
						end;
					end;
				end, 
				DUPCLOSURE = function(unknownValue, unknownValue)
					local hasDebugName = false;
					local dupClosureIndexByte = bit32.band(bit32.rshift(unknownValue, 8), 255);
					local constantIndex = bit32.band(bit32.rshift(unknownValue, 16), 65535);
					local constant = constants[constantIndex];
					local protoInfo = protos[constant.value] or {
						debug_name = ("INVALIDPROTO<%*>%*"):format(dataTypeNames[constant.type] or "NONE", constant.value), 
						params_count = 0, 
						line_defined = -1, 
						stack_size = 0, 
						type_info = {}, 
						upvalues_count = 0, 
						is_vararg = false, 
						protos = {}, 
						constants = {}, 
						flags = 0, 
						code = {}, 
						line_info = {}, 
						abs_line_info = {}
					};
					local _ = currentLine;
					local funcString, debugNameValue;
					if protoInfo then
						debugNameValue = protoInfo.debug_name;
						hasDebugName = debugNameValue;
					end;
					if not hasDebugName then
						funcString = "func";
						uniqueIdCounter = uniqueIdCounter + 1;
						debugNameValue = funcString .. uniqueIdCounter;
					end;
					hasDebugName = false;
					protoInfo.debug_name = debugNameValue;
					disassembleProto(protoInfo, unknownValue);
					table.insert(disassemblyLines, currentLine);
					currentLine = indentation .. "[" .. instructionCounter - 1 .. "] #" .. arrayIndex;
					debugPrintInstruction("DUPCLOSURE " .. dupClosureIndexByte .. ", " .. constantIndex, "var" .. dupClosureIndexByte .. " = " .. protoInfo.debug_name);
					debugNameValue = 0;
					while true do
						funcString = code[instructionCounter];
						if funcString and opcodeMapByCode[bit32.band(funcString, 255)].opname == "CAPTURE" then
							table.insert(disassemblyLines, currentLine);
							currentLine = indentation .. "[" .. instructionCounter - 1 .. "] #" .. arrayIndex;
							local captureType2 = bit32.band(bit32.rshift(funcString, 8), 255);
							local variableIndex2 = bit32.band(bit32.rshift(funcString, 16), 255);
							if captureType2 == 0 then
								debugPrintInstruction("CAPTURE " .. captureType2 .. ", " .. variableIndex2, "up" .. debugNameValue .. " = var" .. variableIndex2 .. " -- Readable");
							elseif captureType2 == 1 then
								debugPrintInstruction("CAPTURE " .. captureType2 .. ", " .. variableIndex2, "up" .. debugNameValue .. " = var" .. variableIndex2 .. " -- Readable and writable");
							elseif captureType2 == 2 then
								debugPrintInstruction("CAPTURE " .. captureType2 .. ", " .. variableIndex2, "up" .. debugNameValue .. " = up" .. variableIndex2);
							else
								debugPrintInstruction("CAPTURE " .. captureType2 .. ", " .. variableIndex2, "up" .. debugNameValue .. " = ??? -- Invalid capture type");
							end;
							instructionCounter = instructionCounter + 1;
							debugNameValue = debugNameValue + 1;
						else
							break;
						end;
					end;
				end, 
				PREPVARARGS = function(sourceValue1, _)
					local varArgsCountByte2 = bit32.band(bit32.rshift(sourceValue1, 8), 255);
					local varargCount = varArgsCountByte2 - 1;
					if varargCount == 0 then
						debugPrintInstruction("PREPVARARGS " .. varArgsCountByte2, "-- No varargs");
						return;
					elseif varargCount == -1 then
						debugPrintInstruction("PREPVARARGS " .. varArgsCountByte2, "-- Prepare for any number (top) of variables as ...");
						return;
					else
						debugPrintInstruction("PREPVARARGS " .. varArgsCountByte2, "-- Prepare for " .. varargCount .. " variables as ...");
						return;
					end;
				end, 
				LOADKX = function(sourceValue2, _)
					local loadKIndexByte = bit32.band(bit32.rshift(sourceValue2, 8), 255);
					local constantIndex = code[instructionCounter];
					if instructionCounter == codeLength then
						error("Corrupted aux");
					end;
					instructionCounter = instructionCounter + 1;
					local localConstantIndex = constantIndex;
					debugPrintInstruction("LOADK " .. loadKIndexByte .. " [" .. localConstantIndex .. "]", "var" .. loadKIndexByte .. " = " .. constantToString(constants[localConstantIndex]));
				end, 
				JUMPX = function(sourceValue3, _)
					buffer.writeu32(buffer8, 0, (bit32.rshift(sourceValue3, 8)));
					local jumpOffset = bit32.rshift(buffer.readi32(buffer8, 1), 16);
					local targetInstructionIndex = instructionCounter + jumpOffset;
					local jumpTargets = jumpTable[targetInstructionIndex];
					if jumpTargets then
						table.insert(jumpTargets, instructionCounter - 1);
					else
						jumpTable[targetInstructionIndex] = {
							instructionCounter - 1
						};
					end;
					debugPrintInstruction("JUMPX " .. jumpOffset, "-- goto [" .. targetInstructionIndex .. "]");
				end, 
				COVERAGE = function(unknownValue, _)
					debugPrintInstruction("COVERAGE " .. bit32.rshift(unknownValue, 8), "instruction_hits[" .. instructionIndex .. "] += 1");
				end, 
				CAPTURE = function(captureValue, _)
					debugPrintInstruction("CAPTURE " .. bit32.band(bit32.rshift(captureValue, 8), 255) .. ", " .. bit32.band(bit32.rshift(captureValue, 16), 255), "-- Should not exist here, but equivelant to NOP");
				end, 
				SUBRK = function(instructionData, _)
					local registerA = bit32.band(bit32.rshift(instructionData, 8), 255);
					local registerB = bit32.band(bit32.rshift(instructionData, 16), 255);
					local bandedValue9 = bit32.band(bit32.rshift(instructionData, 24), 255);
					local constantValue = constants[registerB];
					debugPrintInstruction("SUBRK " .. registerA .. ", " .. registerB .. ", " .. bandedValue9, "var" .. registerA .. " = " .. constantToString(constantValue) .. " - var" .. bandedValue9);
				end, 
				DIVRK = function(instructionData, _)
					local registerA = bit32.band(bit32.rshift(instructionData, 8), 255);
					local registerB = bit32.band(bit32.rshift(instructionData, 16), 255);
					local bandedValue10 = bit32.band(bit32.rshift(instructionData, 24), 255);
					local constantValue = constants[registerB];
					debugPrintInstruction("DIVRK " .. registerA .. ", " .. registerB .. ", " .. bandedValue10, "var" .. registerA .. " = " .. constantToString(constantValue) .. " // var" .. bandedValue10);
				end, 
				FASTCALL = function(unknownValue, _)
					local registerA = bit32.band(bit32.rshift(unknownValue, 8), 255);
					local jumpOffset = bit32.band(bit32.rshift(unknownValue, 24), 255);
					local fastcallFunctionName = mathAndBit32FunctionsList[registerA] or "<invalid>";
					local targetInstructionIndex = instructionCounter + jumpOffset;
					local nextInstructionIndex = targetInstructionIndex + 1;
					local jumpTargets = jumpTable[nextInstructionIndex];
					if jumpTargets then
						table.insert(jumpTargets, instructionCounter - 1);
					else
						jumpTable[nextInstructionIndex] = {
							instructionCounter - 1
						};
					end;
					debugPrintInstruction("FASTCALL " .. registerA .. ", " .. jumpOffset, "... = " .. fastcallFunctionName .. "(...) -- Uses args and results from call at [" .. targetInstructionIndex .. "]. If successful, goto [" .. targetInstructionIndex + 1 .. "]");
				end, 
				FASTCALL1 = function(instructionData, _)
					local registerA = bit32.band(bit32.rshift(instructionData, 8), 255);
					local registerB = bit32.band(bit32.rshift(instructionData, 16), 255);
					local jumpOffset = bit32.band(bit32.rshift(instructionData, 24), 255);
					local fastcallFunctionName = mathAndBit32FunctionsList[registerA] or "<invalid>";
					local targetInstructionIndex = instructionCounter + jumpOffset;
					local nextInstructionIndex = targetInstructionIndex + 1;
					local jumpTargets = jumpTable[nextInstructionIndex];
					if jumpTargets then
						table.insert(jumpTargets, instructionCounter - 1);
					else
						jumpTable[nextInstructionIndex] = {
							instructionCounter - 1
						};
					end;
					debugPrintInstruction("FASTCALL1 " .. registerA .. ", " .. registerB .. ", " .. jumpOffset, "... = " .. fastcallFunctionName .. "(var" .. registerB .. ") -- Uses results from call at [" .. targetInstructionIndex .. "]. If successful, goto [" .. targetInstructionIndex + 1 .. "]");
				end, 
				FASTCALL2 = function(instructionData, _)
					local registerA = bit32.band(bit32.rshift(instructionData, 8), 255);
					local registerB = bit32.band(bit32.rshift(instructionData, 16), 255);
					local jumpOffset = bit32.band(bit32.rshift(instructionData, 24), 255);
					local argumentIndex = code[instructionCounter];
					if instructionCounter == codeLength then
						error("Corrupted aux");
					end;
					instructionCounter = instructionCounter + 1;
					argumentIndex = bit32.band(argumentIndex, 255);
					local fastCallResult = mathAndBit32FunctionsList[registerA] or "<invalid>";
					local targetInstructionIndex = instructionCounter + jumpOffset - 1;
					local nextInstructionIndex = targetInstructionIndex + 1;
					local jumpTargets = jumpTable[nextInstructionIndex];
					if jumpTargets then
						table.insert(jumpTargets, instructionCounter - 1);
					else
						jumpTable[nextInstructionIndex] = {
							instructionCounter - 1
						};
					end;
					debugPrintInstruction("FASTCALL2 " .. registerA .. ", " .. registerB .. ", " .. jumpOffset, "... = " .. fastCallResult .. "(var" .. registerB .. ", var" .. argumentIndex .. ") -- Uses results from call at [" .. targetInstructionIndex .. "]. If successful, goto [" .. targetInstructionIndex + 1 .. "]");
				end, 
				FASTCALL2K = function(instructionData, _)
					local registerA = bit32.band(bit32.rshift(instructionData, 8), 255);
					local registerB = bit32.band(bit32.rshift(instructionData, 16), 255);
					local jumpOffset = bit32.band(bit32.rshift(instructionData, 24), 255);
					local fastCallIndex = code[instructionCounter];
					if instructionCounter == codeLength then
						error("Corrupted aux");
					end;
					instructionCounter = instructionCounter + 1;
					local constantIndexLocal = fastCallIndex;
					fastCallIndex = constants[constantIndexLocal];
					local fastCallResultK = mathAndBit32FunctionsList[registerA] or "<invalid>";
					local gotoAddress1 = instructionCounter + jumpOffset - 1;
					local nextAddress1 = gotoAddress1 + 1;
					local addressList1 = jumpTable[nextAddress1];
					if addressList1 then
						table.insert(addressList1, instructionCounter - 1);
					else
						jumpTable[nextAddress1] = {
							instructionCounter - 1
						};
					end;
					debugPrintInstruction("FASTCALL2K " .. registerA .. ", " .. registerB .. ", " .. jumpOffset .. " [" .. constantIndexLocal .. "]", "... = " .. fastCallResultK .. "(var" .. registerB .. ", " .. constantToString(fastCallIndex) .. ") -- Uses results from call at [" .. gotoAddress1 .. "]. If successful, goto [" .. gotoAddress1 + 1 .. "]");
				end, 
				FASTCALL3 = function(instructionData, _)
					local fastCallIndex2 = bit32.band(bit32.rshift(instructionData, 8), 255);
					local argument1Index = bit32.band(bit32.rshift(instructionData, 16), 255);
					local argument2Index = bit32.band(bit32.rshift(instructionData, 24), 255);
					local argumentIndex = code[instructionCounter];
					if instructionCounter == codeLength then
						error("Corrupted aux");
					end;
					instructionCounter = instructionCounter + 1;
					local bitmaskLocal = argumentIndex;
					argumentIndex = bit32.band(bitmaskLocal, 255);
					local shiftedValue = bit32.rshift(bit32.band(bitmaskLocal, 65280), 8);
					local fastCallResult3 = mathAndBit32FunctionsList[fastCallIndex2] or "<invalid>";
					local gotoAddress2 = instructionCounter + argument2Index - 1;
					local nextAddress2 = gotoAddress2 + 1;
					local addressList2 = jumpTable[nextAddress2];
					if addressList2 then
						table.insert(addressList2, instructionCounter - 1);
					else
						jumpTable[nextAddress2] = {
							instructionCounter - 1
						};
					end;
					debugPrintInstruction("FASTCALL3 " .. fastCallIndex2 .. ", " .. argument1Index .. ", " .. argument2Index, "... = " .. fastCallResult3 .. "(var" .. argument1Index .. ", var" .. argumentIndex .. ", var" .. shiftedValue .. ") -- Uses results from call at [" .. gotoAddress2 .. "]. If successful, goto [" .. gotoAddress2 + 1 .. "]");
				end, 
				JUMPXEQKNIL = function(valueToShift1, _)
					local variableIndex1 = bit32.band(bit32.rshift(valueToShift1, 8), 255);
					local jumpOffset1 = bit32.band(bit32.rshift(valueToShift1, 16), 65535);
					local isNotNilCheck1 = code[instructionCounter];
					if instructionCounter == codeLength then
						error("Corrupted aux");
					end;
					instructionCounter = instructionCounter + 1;
					local jumpConditionValue = isNotNilCheck1;
					isNotNilCheck1 = false;
					if bit32.band(jumpConditionValue, 2147483648) > 0 then
						isNotNilCheck1 = true;
					end;
					local gotoAddress3 = instructionCounter - 1 + jumpOffset1;
					local addressList3 = jumpTable[gotoAddress3];
					if addressList3 then
						table.insert(addressList3, instructionCounter - 2);
					else
						jumpTable[gotoAddress3] = {
							instructionCounter - 2
						};
					end;
					debugPrintInstruction("JUMPXEQKNIL " .. variableIndex1 .. ", " .. jumpOffset1 .. " [0x" .. (hexLookupTable[bit32.rshift(jumpConditionValue, 24)] .. hexLookupTable[bit32.band(bit32.rrotate(jumpConditionValue, 16), 255)] .. hexLookupTable[bit32.band(bit32.rrotate(jumpConditionValue, 8), 255)] .. hexLookupTable[bit32.band(jumpConditionValue, 255)]) .. "]", "if var" .. variableIndex1 .. " " .. (isNotNilCheck1 and "~" or "=") .. "= nil then goto [" .. gotoAddress3 .. "] end");
				end, 
				JUMPXEQKB = function(valueToShift2, _)
					local variableIndex2 = bit32.band(bit32.rshift(valueToShift2, 8), 255);
					local jumpOffset2 = bit32.band(bit32.rshift(valueToShift2, 16), 65535);
					local isNotBooleanCheck = code[instructionCounter];
					if instructionCounter == codeLength then
						error("Corrupted aux");
					end;
					instructionCounter = instructionCounter + 1;
					local jumpConditionValueBoolean = isNotBooleanCheck;
					isNotBooleanCheck = false;
					if bit32.band(jumpConditionValueBoolean, 2147483648) > 0 then
						isNotBooleanCheck = true;
					end;
					local gotoAddress4 = instructionCounter - 1 + jumpOffset2;
					local addressList4 = jumpTable[gotoAddress4];
					if addressList4 then
						table.insert(addressList4, instructionCounter - 2);
					else
						jumpTable[gotoAddress4] = {
							instructionCounter - 2
						};
					end;
					debugPrintInstruction("JUMPXEQKB " .. variableIndex2 .. ", " .. jumpOffset2 .. " [0x" .. (hexLookupTable[bit32.rshift(jumpConditionValueBoolean, 24)] .. hexLookupTable[bit32.band(bit32.rrotate(jumpConditionValueBoolean, 16), 255)] .. hexLookupTable[bit32.band(bit32.rrotate(jumpConditionValueBoolean, 8), 255)] .. hexLookupTable[bit32.band(jumpConditionValueBoolean, 255)]) .. "]", "if var" .. variableIndex2 .. " " .. (isNotBooleanCheck and "~" or "=") .. "= " .. (bit32.band(jumpConditionValueBoolean, 1) == 0 and "false" or "true") .. " then goto [" .. gotoAddress4 .. "] end");
				end, 
				JUMPXEQKN = function(valueToShift3, _)
					local variableIndex3 = bit32.band(bit32.rshift(valueToShift3, 8), 255);
					local jumpOffset3 = bit32.band(bit32.rshift(valueToShift3, 16), 65535);
					local isNotEqualCheck = code[instructionCounter];
					if instructionCounter == codeLength then
						error("Corrupted aux");
					end;
					instructionCounter = instructionCounter + 1;
					local localIsNotEqualCheck = isNotEqualCheck;
					isNotEqualCheck = false;
					if bit32.band(localIsNotEqualCheck, 2147483648) > 0 then
						isNotEqualCheck = true;
					end;
					local constantValue1 = constants[bit32.band(localIsNotEqualCheck, 16777215)];
					local gotoAddress5 = instructionCounter - 1 + jumpOffset3;
					local addressList5 = jumpTable[gotoAddress5];
					if addressList5 then
						table.insert(addressList5, instructionCounter - 2);
					else
						jumpTable[gotoAddress5] = {
							instructionCounter - 2
						};
					end;
					debugPrintInstruction("JUMPXEQKN " .. variableIndex3 .. ", " .. jumpOffset3 .. " [0x" .. (hexLookupTable[bit32.rshift(localIsNotEqualCheck, 24)] .. hexLookupTable[bit32.band(bit32.rrotate(localIsNotEqualCheck, 16), 255)] .. hexLookupTable[bit32.band(bit32.rrotate(localIsNotEqualCheck, 8), 255)] .. hexLookupTable[bit32.band(localIsNotEqualCheck, 255)]) .. "]", "if var" .. variableIndex3 .. " " .. (isNotEqualCheck and "~" or "=") .. "= " .. constantToString(constantValue1) .. " then goto [" .. gotoAddress5 .. "] end");
				end, 
				JUMPXEQKS = function(valueToShift4, _)
					local variableIndex4 = bit32.band(bit32.rshift(valueToShift4, 8), 255);
					local jumpOffset4 = bit32.band(bit32.rshift(valueToShift4, 16), 65535);
					local isJumpXEQKS = code[instructionCounter];
					if instructionCounter == codeLength then
						error("Corrupted aux");
					end;
					instructionCounter = instructionCounter + 1;
					local isJumpXEQKSLocal = isJumpXEQKS;
					isJumpXEQKS = false;
					if bit32.band(isJumpXEQKSLocal, 2147483648) > 0 then
						isJumpXEQKS = true;
					end;
					local constantValue2 = constants[bit32.band(isJumpXEQKSLocal, 16777215)];
					local jumpTargetAddress = instructionCounter - 1 + jumpOffset4;
					local jumpTargetTable = jumpTable[jumpTargetAddress];
					if jumpTargetTable then
						table.insert(jumpTargetTable, instructionCounter - 2);
					else
						jumpTable[jumpTargetAddress] = {
							instructionCounter - 2
						};
					end;
					debugPrintInstruction("JUMPXEQKS " .. variableIndex4 .. ", " .. jumpOffset4 .. " [0x" .. (hexLookupTable[bit32.rshift(isJumpXEQKSLocal, 24)] .. hexLookupTable[bit32.band(bit32.rrotate(isJumpXEQKSLocal, 16), 255)] .. hexLookupTable[bit32.band(bit32.rrotate(isJumpXEQKSLocal, 8), 255)] .. hexLookupTable[bit32.band(isJumpXEQKSLocal, 255)]) .. "]", "if var" .. variableIndex4 .. " " .. (isJumpXEQKS and "~" or "=") .. "= " .. constantToString(constantValue2) .. " then goto [" .. jumpTargetAddress .. "] end");
				end, 
				IDIV = function(idivCombinedValue, _)
					local idivVarIndex = bit32.band(bit32.rshift(idivCombinedValue, 8), 255);
					local idivVarIndex2 = bit32.band(bit32.rshift(idivCombinedValue, 16), 255);
					local bandedValue11 = bit32.band(bit32.rshift(idivCombinedValue, 24), 255);
					if idivVarIndex == idivVarIndex2 then
						debugPrintInstruction("IDIV " .. idivVarIndex .. ", " .. idivVarIndex2 .. ", " .. bandedValue11, "var" .. idivVarIndex .. " //= var" .. bandedValue11);
						return;
					else
						debugPrintInstruction("IDIV " .. idivVarIndex .. ", " .. idivVarIndex2 .. ", " .. bandedValue11, "var" .. idivVarIndex .. " = var" .. idivVarIndex2 .. " // var" .. bandedValue11);
						return;
					end;
				end, 
				IDIVK = function(idivkCombinedValue, _)
					local idivkVarIndex = bit32.band(bit32.rshift(idivkCombinedValue, 8), 255);
					local idivkVarIndex2 = bit32.band(bit32.rshift(idivkCombinedValue, 16), 255);
					local idivkConstantIndex = bit32.band(bit32.rshift(idivkCombinedValue, 24), 255);
					local constantValue3 = constants[idivkConstantIndex];
					if idivkVarIndex == idivkVarIndex2 then
						debugPrintInstruction("IDIVK " .. idivkVarIndex .. ", " .. idivkVarIndex2 .. ", " .. idivkConstantIndex, "var" .. idivkVarIndex .. " //= " .. constantToString(constantValue3));
						return;
					else
						debugPrintInstruction("IDIVK " .. idivkVarIndex .. ", " .. idivkVarIndex2 .. ", " .. idivkConstantIndex, "var" .. idivkVarIndex .. " = var" .. idivkVarIndex2 .. " // " .. constantToString(constantValue3));
						return;
					end;
				end
			};
			local opcodeHandlers = {};
			for opcodeName, opcodeHandler in pairs(opCodeHandlers) do
				if opcodeMapByName[opcodeName] then
					local opcodeInfo = opcodeMapByName[opcodeName];
					if not opcodeInfo then
						error((("Unknown opname %*"):format(opcodeName)));
					end;
					opcodeHandlers[opcodeInfo.opcode] = opcodeHandler;
				end;
			end;
			local lineOpcodeMap = {};
			local originalValue = indentation;
			if not isMainProto then
				local indentationLevelLocal = indentation;
				indentation = string.rep("\t", #indentationLevelLocal + 1);
			end;
			if codeLength > 0 then
				while instructionCounter <= codeLength do
					local bytecode = code[instructionCounter];
					instructionIndex = instructionCounter;
					instructionCounter = instructionCounter + 1;
					arrayIndex = arrayIndex + 1;
					local opcodeHandlerFunction = opcodeHandlers[bit32.band(bytecode, 255)];
					if opcodeHandlerFunction then
						currentLine = indentation .. "[" .. instructionCounter - 1 .. "] #" .. arrayIndex;
						opcodeHandlerFunction(bytecode, indentation);
						table.insert(disassemblyLines, currentLine);
						lineOpcodeMap[currentLine] = instructionIndex;
					else
						table.insert(disassemblyLines, (("%*; Unknown opcode 0x%* (%* reversed: 0x%*)\n"):format(indentation, hexLookupTable[bit32.band(bytecode, 255)], opcodeEncodingType, hexLookupTable[opcodeReverseMap[bit32.band(bytecode, 255)]])));
						globalFailedInstructionsCount = globalFailedInstructionsCount + 1;
					end;
				end;
			end;
			currentLine = "";
			indentation = originalValue;
			for lineIndex, callStackEntry in ipairs(disassemblyLines) do
				local lineJumpTargets = jumpTable[lineOpcodeMap[callStackEntry]];
				if lineJumpTargets then
					disassemblyLines[lineIndex] = indentation .. "::" .. table.concat(lineJumpTargets, "::, ::") .. "::\n" .. callStackEntry;
				end;
			end;
			if not isMainProto then
				table.insert(disassemblyLines, indentation .. "end\n");
			end;
			return;
		end;
	end;
	disassembleProto(mainProto, "");
	table.insert(disassemblyLines, 1, (("-- Disassembled with Konstant V%*'s disassembler, made by plusgiant5\n\t\t-- Disassembled on %*\n\t\t-- Luau version %*%*\n\t\t-- Time taken: %* seconds\n\n"):format("2.1", os.date("%Y-%m-%d %H:%M:%S"), luauVersion, if luauTypesVersion then (", Types version %*"):format(luauTypesVersion) else "", (string.format("%.6f", clockFunction() - startTime)))));
	currentLine = "";
	local bufferSize = 0;
	for _, stringLength in ipairs(disassemblyLines) do
		bufferSize = bufferSize + #stringLength;
	end;
	local outputBuffer = buffer.create(bufferSize - 1);
	local bufferOffset = 0;
	for callStackIndex, lineText in ipairs(disassemblyLines) do
		if callStackIndex == #disassemblyLines then
			lineText = string.sub(lineText, 1, #lineText - 1);
		end;
		buffer.writestring(outputBuffer, bufferOffset, lineText);
		bufferOffset = bufferOffset + #lineText;
	end;
	return buffer.tostring(outputBuffer);
end;
local prefixError = disassemblerSettings.prefix_error;
local prefixWarning = disassemblerSettings.prefix_warning;
local prefixInformation = disassemblerSettings.prefix_information;
local function decompileBytecode(decompiledScriptSource, existingTable)
	local startTime = clockFunction();
	startTime = startTime;
	local benchmarkTimer = benchmarkFactory();
	benchmarkTimer:start_benchmark("Global Initialization");
	globalFailedInstructionsCount = 0;
	local settings = existingTable or {};
	assert(settings);
	for settingKey in pairs(settings) do
		if disassemblerSettings[settingKey] == nil then
			error("Unknown setting \"" .. tostring(settingKey) .. "\"");
		end;
	end;
	for settingName, settingValue in pairs(disassemblerSettings) do
		if settings[settingName] == nil then
			settings[settingName] = settingValue;
		end;
	end;
	if settings.smart_var_level then
		assert(settings.smart_var_level == math.floor(settings.smart_var_level), "Expected `smart_var_level` to be an integer");
		assert(settings.smart_var_level >= 0, "Expected `smart_var_level` to be >= 0");
		assert(settings.smart_var_level <= 3, "Expected `smart_var_level` to be <= 3");
	end;
	local prefixErrorValue = settings.prefix_error;
	assert(prefixErrorValue);
	if string.match(prefixErrorValue, "\n") then
		error("Cannot have newline characters in `prefix_error`");
	end;
	prefixError = prefixErrorValue;
	local prefixWarning = settings.prefix_warning;
	assert(prefixWarning);
	if string.match(prefixWarning, "\n") then
		error("Cannot have newline characters in `prefix_warning`");
	end;
	prefixWarning = prefixWarning;
	local prefixInformation = settings.prefix_information;
	assert(prefixInformation);
	if string.match(prefixInformation, "\n") then
		error("Cannot have newline characters in `prefix_information`");
	end;
	prefixInformation = prefixInformation;
	if string.byte(decompiledScriptSource, 1, 1) == 0 then
		local decompilationHeader = ("-- Decompiled with Konstant%*, a fast Luau decompiler made in Luau by plusgiant5\n"):format("2.1");
		if type(decompiledScriptSource) ~= "string" and getScriptHashFunction then
			decompilationHeader = decompilationHeader .. ("-- Script hash: %*\n"):format((string.upper(getScriptHashFunction(decompiledScriptSource))));
		end;
		return (((decompilationHeader .. ("-- Decompiled on %*\n"):format((os.date("%Y-%m-%d %H:%M:%S")))) .. ("-- Time taken: %* seconds\n"):format((string.format("%.6f", clockFunction() - startTime)))) .. "\n-- Target script didn't compile. Compilation error below:\n") .. "--[[\n" .. string.sub(decompiledScriptSource, 2, #decompiledScriptSource) .. "\n]]";
	else
		local bytecodeChunk, bytecodeConstants, _, luauTypesVersion = decodeFunction(decompiledScriptSource);
		decodedValue2 = bytecodeConstants;
		local globalNameCache = {};
		local noticesList = {};
		local function _(chunkType, chunkContent)
			table.insert(noticesList, {
				type = chunkType, 
				content = chunkContent
			});
		end;
		local constantMap = {};
		local function _(expression)
			while expression.t == "name" do
				local overrideExpression = expression.name.override_expr;
				if overrideExpression then
					expression = overrideExpression;
				else
					break;
				end;
			end;
			return expression;
		end;
		local function _(expression, comparisonString)
			local expressionLocal = expression;
			while expressionLocal.t == "name" do
				local overrideExpression2 = expressionLocal.name.override_expr;
				if overrideExpression2 then
					expressionLocal = overrideExpression2;
				else
					break;
				end;
			end;
			expression = expressionLocal;
			if expression.t == "name" then
				return expression.name == comparisonString;
			else
				return false;
			end;
		end;
		local function _(expressionData)
			local variableNumber = expressionData.var_num;
			assert(variableNumber);
			return variableNumber;
		end;
		local function _(unknownObject)
			local variableList = unknownObject.var_list;
			assert(variableList);
			return variableList;
		end;
		local function _(allContributors, contributorList)
			for contributorKey in pairs(contributorList.contributors) do
				allContributors.contributors[contributorKey] = true;
			end;
			allContributors.contributors[contributorList] = true;
		end;
		local function _(readVariable1, readVariable2)
			table.insert(readVariable1.reads, readVariable2);
			table.insert(readVariable2.reads, readVariable1);
		end;
		local function _(writeVariable1, writeVariable2)
			table.insert(writeVariable1.writes, writeVariable2);
			table.insert(writeVariable2.writes, writeVariable1);
		end;
		local function removeReadDependency(readingNode, readNode)
			local readDependencyIndex = table.find(readingNode.reads, readNode);
			assert(readDependencyIndex);
			table.remove(readingNode.reads, readDependencyIndex);
			local readIndex = table.find(readNode.reads, readingNode);
			assert(readIndex);
			table.remove(readNode.reads, readIndex);
		end;
		local function removeWriteDependency(writingNode, writeNode)
			local writeDependencyIndex = table.find(writingNode.writes, writeNode);
			assert(writeDependencyIndex);
			table.remove(writingNode.writes, writeDependencyIndex);
			local writeIndex = table.find(writeNode.writes, writingNode);
			assert(writeIndex);
			table.remove(writeNode.writes, writeIndex);
		end;
		local function addDependency(dependentNode, dependencyNode)
			for _, readDependency in ipairs(dependencyNode.reads) do
				table.insert(dependentNode.reads, readDependency);
				table.insert(readDependency.reads, dependentNode);
			end;
			for _, writeDependency in ipairs(dependencyNode.writes) do
				table.insert(dependentNode.writes, writeDependency);
				table.insert(writeDependency.writes, dependentNode);
			end;
		end;
		local function removeDependencies(nodeToRemoveDependenciesFrom, valueNode)
			for _, readNodeToRemove in ipairs(valueNode.reads) do
				removeReadDependency(nodeToRemoveDependenciesFrom, readNodeToRemove);
			end;
			for _, writeNodeToRemove in ipairs(valueNode.writes) do
				removeWriteDependency(nodeToRemoveDependenciesFrom, writeNodeToRemove);
			end;
		end;
		local variableCounter = 1;
		local luauTypeNames = {
			[0] = "nil", 
			[1] = "boolean", 
			[2] = "number", 
			[3] = "string", 
			[4] = "table", 
			[5] = "function", 
			[6] = "thread", 
			[7] = "userdata", 
			[8] = "vector", 
			[9] = "buffer", 
			[15] = "any"
		};
		local function _(typeInfoIndex)
			local isOptionalType = bit32.band(typeInfoIndex, 128) > 0;
			local typeInfo = luauTypeNames[typeInfoIndex];
			print("Type info", typeInfoIndex, "is", typeInfo);
			if typeInfo then
				return {
					type = typeInfo, 
					optional = isOptionalType
				};
			else
				return {
					type = "invalid", 
					optional = false
				};
			end;
		end;
		local function _(conditionVariable)
			conditionVariable.condition = comparisonOperatorMap[conditionVariable.condition];
		end;
		local linesList = {};
		local function createCommentObject(commentText)
			return {
				t = "comment", 
				lines = linesList, 
				reads = {}, 
				writes = {}, 
				text = commentText, 
				stack = 1
			};
		end;
		local variableMap = {};
		local argumentCounter = 1;
		local function _(index)
			return variableMap[index];
		end;
		local function _(variableName)
			local variableInfo = variableMap[variableName];
			assert(variableInfo);
			return variableInfo;
		end;
		local variableIndexMap = {};
		local function _(variableName)
			local variableIndex = variableIndexMap[variableName];
			local uniqueVariableName = variableName;
			local uniqueCounter = variableIndex or 1;
			while variableMap[uniqueVariableName] or globalNameCache[uniqueVariableName] do
				uniqueCounter = uniqueCounter + 1;
				uniqueVariableName = variableName .. "_" .. uniqueCounter;
			end;
			variableIndexMap[variableName] = uniqueCounter;
			return uniqueVariableName;
		end;
		local function allocateVariable(variableIdentifier, variableRegisters, variableList)
			if variableMap[variableIdentifier] and variableIdentifier ~= "_" then
				error((("[alloc] Variable %* already allocated"):format(variableIdentifier)));
			end;
			local variableData = {
				name = variableIdentifier, 
				attributes = {}, 
				reads = {}, 
				writes = {}, 
				registers = variableRegisters, 
				var_num = #variableList + 1, 
				var_list = variableList
			};
			variableMap[variableIdentifier] = variableData;
			table.insert(variableList, variableData);
			return variableMap[variableIdentifier];
		end;
		local function _(variableName)
			local existingVariable = variableMap[variableName];
			if existingVariable then
				return existingVariable;
			else
				return (allocateVariable(variableName, {
					beginning = -1, 
					ending = -1
				}, {}));
			end;
		end;
		local function _(variableData, initialExpression, variableNumber)
			variableData.init_expr = initialExpression;
			variableData.var_num = variableNumber;
		end;
		local function _(variableToFree)
			local variableNameLocal = variableToFree.name;
			if not variableMap[variableNameLocal] then
				error((("[free] Variable %* not allocated"):format(variableToFree.name)));
			end;
			variableMap[variableNameLocal] = nil;
		end;
		local function _(variableToWrite, luauType)
			local variableName = variableToWrite.name;
			if not variableMap[variableName] then
				error((("[write] Variable %* not allocated"):format(variableToWrite.name)));
			end;
			if variableMap[variableName].luau_type and variableMap[variableName].luau_type ~= luauType then
				error((("[write] Attempt to retype %*: %* to %*: %*"):format(variableToWrite.name, variableMap[variableName].luau_type, variableToWrite, luauType)));
			end;
			variableMap[variableName].luau_type = luauType;
		end;
		local function _(variableObject, newVariableName)
			local variableNameLocal2 = variableObject.name;
			if not variableMap[variableNameLocal2] then
				error((("[write] Variable %* not allocated"):format(variableNameLocal2)));
			end;
			variableMap[variableNameLocal2] = nil;
			variableObject.name = newVariableName;
			variableMap[newVariableName] = variableObject;
			variableObject.attributes.renamed = true;
		end;
		local function _(variableObject, overrideExpression)
			local variableNameLocal3 = variableObject.name;
			if not variableMap[variableNameLocal3] then
				error((("[write] Variable %* not allocated"):format(variableNameLocal3)));
			end;
			variableMap[variableNameLocal3] = nil;
			variableObject.override_expr = overrideExpression;
		end;
		local function _()
			local variableName = "var" .. tostring(variableCounter);
			local variableIndex = variableIndexMap[variableName];
			local uniqueVariableName = variableName;
			local uniqueCounter = variableIndex or 1;
			while variableMap[uniqueVariableName] or globalNameCache[uniqueVariableName] do
				uniqueCounter = uniqueCounter + 1;
				uniqueVariableName = variableName .. "_" .. uniqueCounter;
			end;
			variableIndexMap[variableName] = uniqueCounter;
			local localVariable = uniqueVariableName;
			variableCounter = variableCounter + 1;
			return localVariable;
		end;
		local function _(valueToCompare, argument)
			local allocateVariableFunc = allocateVariable;
			local variableName = "var" .. tostring(variableCounter);
			local variableIndex = variableIndexMap[variableName];
			local uniqueVariableName = variableName;
			local uniqueCounter = variableIndex or 1;
			while variableMap[uniqueVariableName] or globalNameCache[uniqueVariableName] do
				uniqueCounter = uniqueCounter + 1;
				uniqueVariableName = variableName .. "_" .. uniqueCounter;
			end;
			variableIndexMap[variableName] = uniqueCounter;
			local uniqueVariableName = uniqueVariableName;
			variableCounter = variableCounter + 1;
			return (allocateVariableFunc(uniqueVariableName, valueToCompare, argument));
		end;
		local _ = function(baseName, _)
			local argumentName = "arg" .. tostring(argumentCounter);
			if settings.exact_argument_names then
				argumentName = baseName .. "_" .. argumentName;
			end;
			local argumentNameLocal = argumentName;
			local variableIndex = variableIndexMap[argumentNameLocal];
			local uniqueArgumentName = argumentNameLocal;
			local uniqueCounter = variableIndex or 1;
			while variableMap[uniqueArgumentName] or globalNameCache[uniqueArgumentName] do
				uniqueCounter = uniqueCounter + 1;
				uniqueArgumentName = argumentNameLocal .. "_" .. uniqueCounter;
			end;
			variableIndexMap[argumentNameLocal] = uniqueCounter;
			argumentName = uniqueArgumentName;
			argumentCounter = argumentCounter + 1;
			return argumentName;
		end;
		local function _(instance, rangeStartEnd, data)
			local allocateVariableFunc = allocateVariable;
			local functionName = instance.name;
			local argumentName = "arg" .. tostring(argumentCounter);
			if settings.exact_argument_names then
				argumentName = functionName .. "_" .. argumentName;
			end;
			local argumentNameLocal = argumentName;
			local variableIndex = variableIndexMap[argumentNameLocal];
			local uniqueArgumentName = argumentNameLocal;
			local uniqueCounter = variableIndex or 1;
			while variableMap[uniqueArgumentName] or globalNameCache[uniqueArgumentName] do
				uniqueCounter = uniqueCounter + 1;
				uniqueArgumentName = argumentNameLocal .. "_" .. uniqueCounter;
			end;
			variableIndexMap[argumentNameLocal] = uniqueCounter;
			argumentName = uniqueArgumentName;
			argumentCounter = argumentCounter + 1;
			allocateVariableFunc = allocateVariableFunc(argumentName, {
				beginning = rangeStartEnd, 
				ending = rangeStartEnd
			}, data);
			allocateVariableFunc.func_name = instance;
			return allocateVariableFunc;
		end;
		local codeIndexMap = {};
		local function createNothingObject()
			return {
				t = "nothing", 
				lines = linesList, 
				reads = {}, 
				writes = {}
			};
		end;
		local function createDefaultMetadata(isNil)
			if isNil then
				return {
					t = "nil", 
					reads = {}, 
					writes = {}, 
					contributors = {}, 
					invisible = true
				};
			else
				return {
					t = "nil", 
					reads = {}, 
					writes = {}, 
					contributors = {}
				};
			end;
		end;
		local function combineMetadata(metadata, argumentList, _)
			local clonedReads = table.clone(metadata.reads);
			local clonedWrites = table.clone(metadata.writes);
			local clonedContributors = table.clone(metadata.contributors);
			for _, dependencyMetadata in ipairs(argumentList) do
				for _, readDependency in ipairs(dependencyMetadata.reads) do
					table.insert(clonedReads, readDependency);
				end;
				for _, write in ipairs(dependencyMetadata.writes) do
					table.insert(clonedWrites, write);
				end;
				for contributor in pairs(dependencyMetadata.contributors) do
					clonedContributors[contributor] = true;
				end;
			end;
			return {
				t = "call", 
				reads = clonedReads, 
				writes = clonedWrites, 
				contributors = clonedContributors, 
				func = metadata, 
				args = argumentList
			};
		end;
		local function createNameMetadata(nameValue)
			return {
				t = "name", 
				reads = {
					nameValue
				}, 
				writes = {}, 
				contributors = {}, 
				name = nameValue
			};
		end;
		local function createNotMetadata(notExpression)
			return {
				t = "not", 
				reads = table.clone(notExpression.reads), 
				writes = table.clone(notExpression.writes), 
				contributors = table.clone(notExpression.contributors), 
				precedence = 2, 
				rhs = notExpression
			};
		end;
		local function createAndMetadata(leftMetadata, expression)
			local andMetadata = {
				t = "and", 
				reads = tableAppendFunction(leftMetadata.reads, expression.reads), 
				writes = tableAppendFunction(leftMetadata.writes, expression.writes)
			};
			local contributors = leftMetadata.contributors;
			local rightContributors = expression.contributors;
			local combinedContributors = table.clone(contributors);
			for contributor in pairs(rightContributors) do
				combinedContributors[contributor] = true;
			end;
			andMetadata.contributors = combinedContributors;
			andMetadata.precedence = 7;
			andMetadata.lhs = leftMetadata;
			andMetadata.rhs = expression;
			return andMetadata;
		end;
		local function createConditionMetadata(baseMetadata, conditionExpression, conditionMetadata)
			if conditionMetadata then
				local conditionExpression = {
					t = "condition", 
					reads = tableAppendFunction(baseMetadata.reads, conditionMetadata.reads), 
					writes = tableAppendFunction(baseMetadata.writes, conditionMetadata.writes)
				};
				local contributors = baseMetadata.contributors;
				local conditionContributors = conditionMetadata.contributors;
				local combinedConditionContributors = table.clone(contributors);
				for contributor in pairs(conditionContributors) do
					combinedConditionContributors[contributor] = true;
				end;
				conditionExpression.contributors = combinedConditionContributors;
				conditionExpression.precedence = 6;
				conditionExpression.condition = conditionExpression;
				conditionExpression.lhs = baseMetadata;
				conditionExpression.rhs = conditionMetadata;
				return conditionExpression;
			else
				return {
					t = "condition", 
					reads = table.clone(baseMetadata.reads), 
					writes = table.clone(baseMetadata.writes), 
					contributors = table.clone(baseMetadata.contributors), 
					precedence = 2, 
					condition = conditionExpression, 
					lhs = baseMetadata
				};
			end;
		end;
		local function createDefineFunctionMetadata(functionLines, functionReference, functionName, functionType, functionPath)
			local defineFunctionMetadata = {
				t = "define function", 
				reads = {}, 
				writes = {}, 
				lines = functionLines, 
				func = functionReference, 
				func_name = functionName, 
				define_function_type = functionType, 
				path = functionPath
			};
			addDependency(defineFunctionMetadata, functionReference);
			return defineFunctionMetadata;
		end;
		local function createReturnMetadata(lineList, valueList)
			return {
				t = "return", 
				lines = lineList, 
				reads = {}, 
				writes = {}, 
				values = valueList
			};
		end;
		local function _(expression)
			if expression.t == "condition" then
				expression.condition = comparisonOperatorMap[expression.condition];
				return expression;
			elseif expression.t == "not" then
				return expression.rhs;
			else
				return (createNotMetadata(expression));
			end;
		end;
		local expressionReuseCache = {};
		local variableMappedLongStringConstants = {};
		local variableMappedLongStringConstantsOrder = {};
		local longStringUsageCounts = {};
		local longStringConstantAlreadyUsedMap = {};
		local longStringVariableCount = 0;
		local linesHadSkippedReturnMap = {};
		local conditionStopPointsMap = {};
		local markedConditionStopPointsCount = 0;
		local function _(index)
			conditionStopPointsMap[index] = true;
			markedConditionStopPointsCount = markedConditionStopPointsCount + 1;
		end;
		local function createAnalysisData()
			return {
				global_failed_instructions_count = globalFailedInstructionsCount, 
				notices = table.clone(noticesList), 
				lines = table.clone(linesList), 
				variable_mapped_long_string_constants = table.clone(variableMappedLongStringConstants), 
				variable_mapped_long_string_constants_order = table.clone(variableMappedLongStringConstantsOrder), 
				long_string_usage_counts = table.clone(longStringUsageCounts), 
				long_string_constant_already_used = table.clone(longStringConstantAlreadyUsedMap), 
				long_string_variable_count = longStringVariableCount, 
				lines_had_skipped_return = table.clone(linesHadSkippedReturnMap), 
				condition_stop_points = table.clone(conditionStopPointsMap), 
				marked_condition_stop_points = markedConditionStopPointsCount
			};
		end;
		local function _(analysisData)
			globalFailedInstructionsCount = analysisData.global_failed_instructions_count;
			noticesList = analysisData.notices;
			linesList = analysisData.lines;
			variableMappedLongStringConstants = analysisData.variable_mapped_long_string_constants;
			variableMappedLongStringConstantsOrder = analysisData.variable_mapped_long_string_constants_order;
			longStringUsageCounts = analysisData.long_string_usage_counts;
			longStringConstantAlreadyUsedMap = analysisData.long_string_constant_already_used;
			longStringVariableCount = analysisData.long_string_variable_count;
			linesHadSkippedReturnMap = analysisData.lines_had_skipped_return;
			conditionStopPointsMap = analysisData.condition_stop_points;
			markedConditionStopPointsCount = analysisData.marked_condition_stop_points;
		end;
		local protectedCall = nil;
		local function executeProto(protoData, parentFunction)
			local benchmark = benchmarkFactory();
			benchmark:start_benchmark("Initialization");
			local localValue = unknownValue;
			unknownValue = protoData;
			argumentCounter = 1;
			local index = 0;
			local constantsList = protoData.constants;
			local protosList = protoData.protos;
			local codeList = protoData.code;
			local codeLength = #codeList;
			local stack = nil;
			if protoData.stack_size > 0 then
				stack = table.create(protoData.stack_size - 1);
				stack[0] = nil;
			else
				stack = {};
			end;
			local environment = {};
			local tempTable = {};
			local instructionIndex = 0;
			local instructionNumber = 0;
			local codeIndexToInstructionNumberMap = {};
			local instructionNumberToCodeIndexMap = {};
			local instructionList = {};
			local lineData = {};
			local jumpTable = {};
			local function processUnknown()
				if lineData.t == "name" then
					warn(debug.traceback());
				end;
				if not lineData.t then
					error("messageWAHTTERSDGXCvlkhx d");
				end;
				table.insert(linesList, lineData);
				constantMap[lineData] = jumpTable;
				lineData = {
					t = "Unknown", 
					lines = linesList, 
					reads = {}, 
					writes = {}
				};
			end;
			local function addLineData(lineIndex, targetTable)
				if lineData.t == "name" then
					warn(debug.traceback());
				end;
				lineIndex = math.max(lineIndex, 1);
				lineData.index = lineIndex;
				table.insert(targetTable or linesList, lineIndex, lineData);
				constantMap[lineData] = jumpTable;
				lineData = {
					t = "Unknown", 
					lines = linesList, 
					reads = {}, 
					writes = {}
				};
			end;
			local function addWarningComment(warningMessage)
				local lastLine = linesList[#linesList];
				if lastLine and lastLine.t == "comment" and string.sub(lastLine.text, 1, #warningMessage) == warningMessage then
					lastLine.stack = lastLine.stack + 1;
					lastLine.text = warningMessage .. " (x" .. lastLine.stack .. ")";
					return;
				else
					lineData = createCommentObject(warningMessage);
					processUnknown();
					return;
				end;
			end;
			local function addWarningLine(unknownValue, expression)
				lineData = createCommentObject(expression);
				addLineData(unknownValue);
			end;
			local setValue = nil;
			local function createValue(valueToSet, _)
				return setValue(valueToSet, (createDefaultMetadata(true)));
			end;
			local function _()
				lineData = {
					t = "nothing", 
					lines = linesList, 
					reads = {}, 
					writes = {}
				};
			end;
			local function defineVariable(variableNames, variableValue)
				if variableValue.varname then
					assert(#variableNames == 1);
					assert(variableNames[1] == variableValue.varname);
				end;
				assert(#variableNames > 0);
				lineData = {
					t = "define variable", 
					lines = linesList, 
					reads = {}, 
					writes = {}, 
					names = variableNames, 
					value = variableValue
				};
				if variableValue.t == "nil" and variableValue.invisible then
					for index, variableName in ipairs(variableNames) do
						variableName.init_expr = variableValue;
						variableName.var_num = index;
					end;
				else
					for index, variableName2 in ipairs(variableNames) do
						variableName2.init_expr = variableValue;
						variableName2.var_num = index;
						local expressionData = lineData;
						table.insert(expressionData.writes, variableName2);
						table.insert(variableName2.writes, expressionData);
					end;
				end;
				addDependency(lineData, variableValue);
			end;
			local setValueWithConversion = nil;
			local variableMap = {};
			local booleanMap = {};
			local function _(key, value)
				variableMap[key] = value;
			end;
			local function _(loopRange)
				for index = loopRange.beginning, loopRange.ending do
					if variableMap[index] then
						variableMap[index] = nil;
					end;
				end;
			end;
			local function _(indexValue)
				return variableMap[indexValue];
			end;
			local function _(trueIndex)
				booleanMap[trueIndex] = true;
			end;
			local function _(nilIndex)
				booleanMap[nilIndex] = nil;
			end;
			local function _(isIndexPresent)
				if booleanMap[isIndexPresent] then
					return true;
				else
					return false;
				end;
			end;
			local function _(variableKey)
				local evaluatedValue = stack[variableKey];
				if not evaluatedValue then
					addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(variableKey) .. "]");
					evaluatedValue = setValue(variableKey, (createDefaultMetadata(true)));
				end;
				return evaluatedValue;
			end;
			local function getValue(variableKey2)
				if variableMap[variableKey2] then
					return setValueWithConversion(variableKey2, variableMap[variableKey2]);
				else
					local cachedValue = stack[variableKey2];
					if not cachedValue then
						addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(variableKey2) .. "]");
						cachedValue = setValue(variableKey2, (createDefaultMetadata(true)));
					end;
					return cachedValue;
				end;
			end;
			local function assignValuesToRange(loopRange, value, values)
				assert(#values > 0);
				local valueIndex = 0;
				for rangeIndex = loopRange.beginning, loopRange.ending do
					valueIndex = valueIndex + 1;
					local currentValue = values[valueIndex];
					assert(currentValue);
					variableMap[rangeIndex] = currentValue;
				end;
				defineVariable(values, value);
				processUnknown();
			end;
			local function assignValuesToRange2(range, values2)
				assert(#values2 > 0);
				local rangeBeginning = range.beginning;
				local initialValue = stack[rangeBeginning];
				assert(initialValue);
				local index = 0;
				for rangeIndex2 = rangeBeginning, range.ending do
					index = index + 1;
					local currentValue2 = values2[index];
					assert(currentValue2);
					variableMap[rangeIndex2] = currentValue2;
				end;
				defineVariable(values2, initialValue);
				processUnknown();
			end;
			local localVariable = nil;
			local function _(key, value)
				stack[key] = value;
			end;
			local function _(expression)
				if expressionReuseCache[expression] then
					addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
					return expression;
				else
					expressionReuseCache[expression] = true;
					return expression;
				end;
			end;
			local function setVariable(variableName, variableValue)
				local variableAssignment = {
					t = "set variable", 
					reads = {}, 
					writes = {}, 
					lines = linesList, 
					name = variableName, 
					value = variableValue
				};
				addDependency(variableAssignment, variableValue);
				table.insert(variableAssignment.writes, variableName);
				table.insert(variableName.writes, variableAssignment);
				lineData = variableAssignment;
			end;
			localVariable = setVariable;
			local function setGlobalFunction(globalName, globalValue)
				local globalAssignmentData = {
					t = "set global", 
					reads = {}, 
					writes = {}, 
					lines = linesList, 
					name = globalName, 
					value = globalValue
				};
				addDependency(globalAssignmentData, globalValue);
				lineData = globalAssignmentData;
			end;
			local function evaluateExpressionFunction(expressionKey, key, value)
				local expressionValue;
				if variableMap[expressionKey] then
					expressionValue = setValueWithConversion(expressionKey, variableMap[expressionKey]);
				else
					local cachedExpressionValue = stack[expressionKey];
					if not cachedExpressionValue then
						addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(expressionKey) .. "]");
						cachedExpressionValue = setValue(expressionKey, (createDefaultMetadata(true)));
					end;
					expressionValue = cachedExpressionValue;
				end;
				if expressionReuseCache[expressionValue] then
					addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
				else
					expressionReuseCache[expressionValue] = true;
				end;
				local localExpressionValue = expressionValue;
				lineData = {
					t = "set table", 
					lines = linesList, 
					reads = {}, 
					writes = {}, 
					table = localExpressionValue, 
					key = key, 
					value = value
				};
				addDependency(lineData, localExpressionValue);
				addDependency(lineData, key);
				addDependency(lineData, value);
			end;
			local function evaluateExpressionsInRangeFunction(startIndex, count)
				local expressionValuesList = {};
				local expressionValuesTrace = createReturnMetadata(linesList, expressionValuesList);
				if count == -1 then
					for expressionIndex = startIndex, endIndex do
						local evaluatedExpression;
						if variableMap[expressionIndex] then
							evaluatedExpression = setValueWithConversion(expressionIndex, variableMap[expressionIndex]);
						else
							local newExpressionValue = stack[expressionIndex];
							if not newExpressionValue then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(expressionIndex) .. "]");
								newExpressionValue = setValue(expressionIndex, (createDefaultMetadata(true)));
							end;
							evaluatedExpression = newExpressionValue;
						end;
						if expressionReuseCache[evaluatedExpression] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[evaluatedExpression] = true;
						end;
						local localEvaluatedExpression = evaluatedExpression;
						addDependency(expressionValuesTrace, localEvaluatedExpression);
						table.insert(expressionValuesList, localEvaluatedExpression);
					end;
				else
					for expressionIndex2 = startIndex, startIndex + count - 1 do
						local evaluatedExpression2;
						if variableMap[expressionIndex2] then
							evaluatedExpression2 = setValueWithConversion(expressionIndex2, variableMap[expressionIndex2]);
						else
							local newExpressionValue2 = stack[expressionIndex2];
							if not newExpressionValue2 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(expressionIndex2) .. "]");
								newExpressionValue2 = setValue(expressionIndex2, (createDefaultMetadata(true)));
							end;
							evaluatedExpression2 = newExpressionValue2;
						end;
						if expressionReuseCache[evaluatedExpression2] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[evaluatedExpression2] = true;
						end;
						local localEvaluatedExpression2 = evaluatedExpression2;
						addDependency(expressionValuesTrace, localEvaluatedExpression2);
						table.insert(expressionValuesList, localEvaluatedExpression2);
					end;
				end;
				lineData = expressionValuesTrace;
			end;
			local function breakStatementFunction()
				lineData = {
					t = "break", 
					lines = linesList, 
					reads = {}, 
					writes = {}
				};
			end;
			local function continueStatement()
				lineData = {
					t = "continue", 
					lines = linesList, 
					reads = {}, 
					writes = {}
				};
			end;
			local function unknownJumpFunction(destination)
				lineData = {
					t = "unknown jump", 
					lines = linesList, 
					reads = {}, 
					writes = {}, 
					destination = destination
				};
			end;
			local function processVariableRegistrationRangeFunction(unknownObject)
				local variableRegistrations = {};
				local localVariableRegistrationRange = unknownObject.var_reg_range;
				local localBeginning = localVariableRegistrationRange.beginning;
				local localEnding = localVariableRegistrationRange.ending;
				if localBeginning == localEnding then
					local expressionList = {};
					local localV1223 = allocateVariable;
					local variableName = "var" .. tostring(variableCounter);
					local existingVariableCount = variableIndexMap[variableName];
					local localVariableName = variableName;
					local variableCounter = existingVariableCount or 1;
					while variableMap[localVariableName] or globalNameCache[localVariableName] do
						variableCounter = variableCounter + 1;
						localVariableName = variableName .. "_" .. variableCounter;
					end;
					variableIndexMap[variableName] = variableCounter;
					local uniqueVariableName = localVariableName;
					variableCounter = variableCounter + 1;
					table.insert(variableRegistrations, (localV1223(uniqueVariableName, localVariableRegistrationRange, expressionList)));
					return variableRegistrations;
				else
					local localTable1 = {};
					for index = localBeginning, localEnding do
						local range = {
							beginning = index, 
							ending = index
						};
						local someFunction = allocateVariable;
						local variableName = "var" .. tostring(variableCounter);
						local variableValue = variableIndexMap[variableName];
						local uniqueVariableNameBase = variableName;
						local variableSuffix = variableValue or 1;
						while variableMap[uniqueVariableNameBase] or globalNameCache[uniqueVariableNameBase] do
							variableSuffix = variableSuffix + 1;
							uniqueVariableNameBase = variableName .. "_" .. variableSuffix;
						end;
						variableIndexMap[variableName] = variableSuffix;
						local uniqueVariableName = uniqueVariableNameBase;
						variableCounter = variableCounter + 1;
						local variableInfo = someFunction(uniqueVariableName, range, localTable1);
						variableInfo.attributes.multireg = true;
						table.insert(variableRegistrations, variableInfo);
					end;
					return variableRegistrations;
				end;
			end;
			local success = true;
			local function lockUnlockVariables()
				local taskDefinitions = tempTable[instructionIndex];
				assert(success);
				success = false;
				if taskDefinitions then
					for index = #taskDefinitions, 1, -1 do
						local taskDefinition = taskDefinitions[index];
						local operationType = taskDefinition.type;
						if operationType == "lockvar" then
							local variableRegisterRange = taskDefinition.var_reg_range;
							for index = variableRegisterRange.beginning, variableRegisterRange.ending do
								booleanMap[index] = true;
							end;
							table.remove(taskDefinitions, index);
						elseif operationType == "unlockvar" then
							local variableRegisterRange = taskDefinition.var_reg_range;
							for index = variableRegisterRange.beginning, variableRegisterRange.ending do
								booleanMap[index] = nil;
							end;
							table.remove(taskDefinitions, index);
						end;
					end;
					if #taskDefinitions == 0 then
						tempTable[instructionIndex] = nil;
					end;
				end;
			end;
			local function processVariable(variableRegister, variableData)
				local taskDefinitions = tempTable[instructionIndex];
				assert(not success);
				success = true;
				local variableName = variableMap[variableRegister];
				if variableName then
					if booleanMap[variableRegister] and true or false then
						local lastSetVariable = linesList[#linesList];
						local variableUpdated = nil;
						if lastSetVariable and lastSetVariable.t == "set variable" and lastSetVariable.name == variableName then
							local variableValue = lastSetVariable.value;
							removeDependencies(lastSetVariable, variableValue);
							local typeString = variableData.t;
							if typeString == "constant index" then
								if variableData.table.t == "name" and variableData.table.name == variableName then
									variableData.table = variableValue;
									variableUpdated = true;
								end;
							elseif typeString == "call" and variableData.func.t == "name" and variableData.func.name == variableName then
								variableData.func = variableValue;
								variableUpdated = true;
							end;
							if variableUpdated then
								lastSetVariable.value = variableData;
								addDependency(lastSetVariable, variableData);
							end;
						end;
						if not variableUpdated then
							setVariable(variableName, variableData);
							processUnknown();
						end;
					else
						local registers = variableName.registers;
						for registerIndex = registers.beginning, registers.ending do
							if variableMap[registerIndex] then
								variableMap[registerIndex] = nil;
							end;
						end;
					end;
				end;
				if taskDefinitions then
					for _, taskDefinition in ipairs(taskDefinitions) do
						local variableRegisterRange = taskDefinition.var_reg_range;
						local registerBeginning = variableRegisterRange.beginning;
						local registerEnding = variableRegisterRange.ending;
						local isRegisterInRange = false;
						if registerBeginning <= variableRegister then
							isRegisterInRange = variableRegister <= registerEnding;
						end;
						if not isRegisterInRange then

						end;
						if taskDefinition.type == "defvar" then
							isRegisterInRange = nil;
							if not taskDefinition.predef then
								registerBeginning = taskDefinition.var_reg_range;
								if registerBeginning.ending - registerBeginning.beginning + 1 == 1 then
									registerBeginning = taskDefinition.var_reg_range.beginning;
									if variableMap[registerBeginning] then
										variableRegisterRange = setValueWithConversion(registerBeginning, variableMap[registerBeginning]);
									else
										local evaluatedExpression = stack[registerBeginning];
										if not evaluatedExpression then
											addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(registerBeginning) .. "]");
											evaluatedExpression = setValue(registerBeginning, (createDefaultMetadata(true)));
										end;
										variableRegisterRange = evaluatedExpression;
									end;
									if variableRegisterRange and variableRegisterRange.varname then
										isRegisterInRange = {
											variableRegisterRange.varname
										};
									end;
								end;
							end;
							isRegisterInRange = isRegisterInRange or processVariableRegistrationRangeFunction(taskDefinition);
							if taskDefinition.no_inline then
								for _, item in ipairs(isRegisterInRange) do
									item.attributes.no_inline = true;
								end;
							end;
							if taskDefinition.predef then
								assignValuesToRange(taskDefinition.var_reg_range, setValue(nil, (createDefaultMetadata(true))), isRegisterInRange);
							else
								assignValuesToRange2(taskDefinition.var_reg_range, isRegisterInRange);
							end;
						else
							addWarningComment(prefixWarning .. ": Skipped task `" .. taskDefinition.type .. "` above");
						end;
					end;
				end;
			end;
			local function definePredefinedVariables()
				local taskDefinitions = tempTable[instructionIndex];
				assert(not success);
				success = true;
				if taskDefinitions then
					for _, taskDefinition in ipairs(taskDefinitions) do
						if taskDefinition.type == "defvar" and taskDefinition.predef then
							assignValuesToRange(taskDefinition.var_reg_range, setValue(nil, (createDefaultMetadata(true))), (processVariableRegistrationRangeFunction(taskDefinition)));
						else
							addWarningComment(prefixWarning .. ": Skipped task `" .. taskDefinition.type .. "` above");
						end;
					end;
				end;
			end;
			local constantValue = nil;
			local function _(expressionIndex, maxValue, namecallMethod)
				local argumentsList = {};
				local expressionValue;
				if variableMap[expressionIndex] then
					expressionValue = setValueWithConversion(expressionIndex, variableMap[expressionIndex]);
				else
					local evaluatedExpression = stack[expressionIndex];
					if not evaluatedExpression then
						addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(expressionIndex) .. "]");
						evaluatedExpression = setValue(expressionIndex, (createDefaultMetadata(true)));
					end;
					expressionValue = evaluatedExpression;
				end;
				if expressionReuseCache[expressionValue] then
					addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
				else
					expressionReuseCache[expressionValue] = true;
				end;
				local localExpressionValue = expressionValue;
				expressionValue = {
					t = "call", 
					reads = {}, 
					writes = {}, 
					lines = linesList, 
					func = localExpressionValue, 
					args = argumentsList, 
					namecall_method = namecallMethod
				};
				addDependency(expressionValue, localExpressionValue);
				local startIndex = namecallMethod and 2 or 1;
				for indexOffset = startIndex, maxValue do
					local currentExpressionIndex = expressionIndex + indexOffset;
					local currentExpressionValue;
					if variableMap[currentExpressionIndex] then
						currentExpressionValue = setValueWithConversion(currentExpressionIndex, variableMap[currentExpressionIndex]);
					else
						local cachedExpression = stack[currentExpressionIndex];
						if not cachedExpression then
							addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(currentExpressionIndex) .. "]");
							cachedExpression = setValue(currentExpressionIndex, (createDefaultMetadata(true)));
						end;
						currentExpressionValue = cachedExpression;
					end;
					if expressionReuseCache[currentExpressionValue] then
						addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
					else
						expressionReuseCache[currentExpressionValue] = true;
					end;
					local localCurrentExpressionValue = currentExpressionValue;
					table.insert(argumentsList, localCurrentExpressionValue);
					addDependency(expressionValue, localCurrentExpressionValue);
				end;
				lineData = expressionValue;
			end;
			setValue = function(expressionKey, newValue)
				if expressionKey then
					local cachedValue = stack[expressionKey];
					if cachedValue then
						environment[cachedValue] = nil;
					end;
					stack[expressionKey] = newValue;
					environment[newValue] = expressionKey;
				end;
				return newValue;
			end;
			local function createVarArgs(varArgsIndex)
				return setValue(varArgsIndex, {
					t = "varargs", 
					reads = {}, 
					writes = {}, 
					contributors = {}
				});
			end;
			local function createGlobal(globalIndex, globalName)
				assert(globalName);
				return setValue(globalIndex, {
					t = "global", 
					reads = {}, 
					writes = {}, 
					contributors = {}, 
					name = globalName
				});
			end;
			local function createConstant(constantIndex, constantValue)
				if not constantValue then
					error("Bad Konstant");
				end;
				if constantValue.type == 3 then
					local constantStringValue = constantValue.value;
					if #constantStringValue > 128 then
						if variableMappedLongStringConstants[constantStringValue] then
							local localStringCounts = longStringUsageCounts;
							localStringCounts[constantStringValue] = localStringCounts[constantStringValue] + 1;
						elseif longStringConstantAlreadyUsedMap[constantStringValue] then
							longStringVariableCount = longStringVariableCount + 1;
							variableMappedLongStringConstants[constantStringValue] = "longstring" .. longStringVariableCount;
							longStringUsageCounts[constantStringValue] = 2;
							table.insert(variableMappedLongStringConstantsOrder, constantStringValue);
						else
							longStringConstantAlreadyUsedMap[constantStringValue] = true;
						end;
					end;
				end;
				return setValue(constantIndex, {
					t = "constant", 
					reads = {}, 
					writes = {}, 
					contributors = {}, 
					const = constantValue
				});
			end;
			local function createNewTable(newTableIndex)
				return setValue(newTableIndex, {
					t = "new table", 
					reads = {}, 
					writes = {}, 
					contributors = {}, 
					initializers = {}, 
					initializers_order = {}
				});
			end;
			local function createConstantIndex(tableIndex, constantTable, tableIndex, nameCall)
				return setValue(tableIndex, {
					t = "constant index", 
					reads = table.clone(constantTable.reads), 
					writes = table.clone(constantTable.writes), 
					contributors = table.clone(constantTable.contributors), 
					table = constantTable, 
					index = tableIndex, 
					namecall = nameCall
				});
			end;
			setValueWithConversion = function(unknownValue, value)
				return setValue(unknownValue, (createNameMetadata(value)));
			end;
			local function createBoolean(booleanValue, booleanValue)
				return setValue(booleanValue, {
					t = "boolean", 
					reads = {}, 
					writes = {}, 
					contributors = {}, 
					value = booleanValue
				});
			end;
			local function createOrExpression(unknownValue, leftExpression, rightExpression)
				local localCreateExpression = setValue;
				local expression = unknownValue;
				local orExpression = {
					t = "or", 
					reads = tableAppendFunction(leftExpression.reads, rightExpression.reads), 
					writes = tableAppendFunction(leftExpression.writes, rightExpression.writes)
				};
				local leftContributors = leftExpression.contributors;
				local rightContributors = rightExpression.contributors;
				local combinedContributors = table.clone(leftContributors);
				for contributor in pairs(rightContributors) do
					combinedContributors[contributor] = true;
				end;
				orExpression.contributors = combinedContributors;
				orExpression.precedence = 8;
				orExpression.lhs = leftExpression;
				orExpression.rhs = rightExpression;
				return localCreateExpression(expression, orExpression);
			end;
			local function createOrExpression(expression1, key, value)
				return setValue(expression1, (createAndMetadata(key, value)));
			end;
			local function createAndExpression(expression1, arg1, arg2, arg3)
				return setValue(expression1, (createConditionMetadata(arg1, arg2, arg3)));
			end;
			local function createAdditionExpression(unknownValue, leftExpression, rightExpression)
				local evaluateExpression = setValue;
				local expression = unknownValue;
				local additionExpression = {
					t = "addition", 
					reads = tableAppendFunction(leftExpression.reads, rightExpression.reads), 
					writes = tableAppendFunction(leftExpression.writes, rightExpression.writes)
				};
				local leftContributors = leftExpression.contributors;
				local rightContributors = rightExpression.contributors;
				local combinedContributors = table.clone(leftContributors);
				for contributor in pairs(rightContributors) do
					combinedContributors[contributor] = true;
				end;
				additionExpression.contributors = combinedContributors;
				additionExpression.precedence = 4;
				additionExpression.lhs = leftExpression;
				additionExpression.rhs = rightExpression;
				return evaluateExpression(expression, additionExpression);
			end;
			local function createSubtractionExpression(unknownValue, leftExpression, rightExpression)
				local evaluateExpression = setValue;
				local expression = unknownValue;
				local subtractionExpression = {
					t = "subtraction", 
					reads = tableAppendFunction(leftExpression.reads, rightExpression.reads), 
					writes = tableAppendFunction(leftExpression.writes, rightExpression.writes)
				};
				local leftContributors = leftExpression.contributors;
				local rightContributors = rightExpression.contributors;
				local combinedContributors = table.clone(leftContributors);
				for contributor in pairs(rightContributors) do
					combinedContributors[contributor] = true;
				end;
				subtractionExpression.contributors = combinedContributors;
				subtractionExpression.precedence = 4;
				subtractionExpression.lhs = leftExpression;
				subtractionExpression.rhs = rightExpression;
				return evaluateExpression(expression, subtractionExpression);
			end;
			local function createMultiplicationExpression(unknownValue, leftExpression, rightExpression)
				local evaluateExpression = setValue;
				local expression = unknownValue;
				local multiplicationExpression = {
					t = "multiplication", 
					reads = tableAppendFunction(leftExpression.reads, rightExpression.reads), 
					writes = tableAppendFunction(leftExpression.writes, rightExpression.writes)
				};
				local leftContributors = leftExpression.contributors;
				local rightContributors = rightExpression.contributors;
				local combinedContributors = table.clone(leftContributors);
				for contributor in pairs(rightContributors) do
					combinedContributors[contributor] = true;
				end;
				multiplicationExpression.contributors = combinedContributors;
				multiplicationExpression.precedence = 3;
				multiplicationExpression.lhs = leftExpression;
				multiplicationExpression.rhs = rightExpression;
				return evaluateExpression(expression, multiplicationExpression);
			end;
			local function createDivisionExpression(unknownValue, leftHandSide, rightExpression)
				local evaluateExpression = setValue;
				local expression = unknownValue;
				local divisionOperation = {
					t = "division", 
					reads = tableAppendFunction(leftHandSide.reads, rightExpression.reads), 
					writes = tableAppendFunction(leftHandSide.writes, rightExpression.writes)
				};
				local leftContributors = leftHandSide.contributors;
				local rightHandSideContributors = rightExpression.contributors;
				local allContributors = table.clone(leftContributors);
				for contributor in pairs(rightHandSideContributors) do
					allContributors[contributor] = true;
				end;
				divisionOperation.contributors = allContributors;
				divisionOperation.precedence = 3;
				divisionOperation.lhs = leftHandSide;
				divisionOperation.rhs = rightExpression;
				return evaluateExpression(expression, divisionOperation);
			end;
			local function createFloorDivisionOperation(unknownValue, leftHandSideOperand, rightExpression)
				local v1375_local = setValue;
				local v1625_local = unknownValue;
				local floorDivisionOperation = {
					t = "floor division", 
					reads = tableAppendFunction(leftHandSideOperand.reads, rightExpression.reads), 
					writes = tableAppendFunction(leftHandSideOperand.writes, rightExpression.writes)
				};
				local leftContributors = leftHandSideOperand.contributors;
				local rightHandSideOperandContributors = rightExpression.contributors;
				local allFloorDivisionContributors = table.clone(leftContributors);
				for contributor in pairs(rightHandSideOperandContributors) do
					allFloorDivisionContributors[contributor] = true;
				end;
				floorDivisionOperation.contributors = allFloorDivisionContributors;
				floorDivisionOperation.precedence = 3;
				floorDivisionOperation.lhs = leftHandSideOperand;
				floorDivisionOperation.rhs = rightExpression;
				return v1375_local(v1625_local, floorDivisionOperation);
			end;
			local function createModulusOperation(unknownValue, leftHandSideModulus, rightExpression)
				local v1375_local = setValue;
				local v1636_local = unknownValue;
				local modulusOperation = {
					t = "modulus", 
					reads = tableAppendFunction(leftHandSideModulus.reads, rightExpression.reads), 
					writes = tableAppendFunction(leftHandSideModulus.writes, rightExpression.writes)
				};
				local leftContributors = leftHandSideModulus.contributors;
				local rightHandSideModulusContributors = rightExpression.contributors;
				local allModulusContributors = table.clone(leftContributors);
				for contributor in pairs(rightHandSideModulusContributors) do
					allModulusContributors[contributor] = true;
				end;
				modulusOperation.contributors = allModulusContributors;
				modulusOperation.precedence = 3;
				modulusOperation.lhs = leftHandSideModulus;
				modulusOperation.rhs = rightExpression;
				return v1375_local(v1636_local, modulusOperation);
			end;
			local function v1649(v1647, argument)
				return setValue(v1647, (createNotMetadata(argument)));
			end;
			local function createNegateOperation(v1650, expression)
				return setValue(v1650, {
					t = "negate", 
					reads = table.clone(expression.reads), 
					writes = table.clone(expression.writes), 
					contributors = table.clone(expression.contributors), 
					precedence = 2, 
					rhs = expression
				});
			end;
			local function createLengthOperation(v1653, expression)
				return setValue(v1653, {
					t = "length", 
					reads = table.clone(expression.reads), 
					writes = table.clone(expression.writes), 
					contributors = table.clone(expression.contributors), 
					precedence = 2, 
					rhs = expression
				});
			end;
			local function createExponentiationOperation(unknownValue, leftHandSideExponent, exponentiationData)
				local v1375_local = setValue;
				local v1656_local = unknownValue;
				local exponentiationOperation = {
					t = "exponentiation", 
					reads = tableAppendFunction(leftHandSideExponent.reads, exponentiationData.reads), 
					writes = tableAppendFunction(leftHandSideExponent.writes, exponentiationData.writes)
				};
				local leftOperandContributors = leftHandSideExponent.contributors;
				local rightHandSideExponentContributors = exponentiationData.contributors;
				local allExponentContributors = table.clone(leftOperandContributors);
				for contributorIndex in pairs(rightHandSideExponentContributors) do
					allExponentContributors[contributorIndex] = true;
				end;
				exponentiationOperation.contributors = allExponentContributors;
				exponentiationOperation.precedence = 1;
				exponentiationOperation.lhs = leftHandSideExponent;
				exponentiationOperation.rhs = exponentiationData;
				return v1375_local(v1656_local, exponentiationOperation);
			end;
			local function createConcatenationExpression(concatenationContext, expressions)
				local readDependencies = {};
				local writeDependencies = {};
				local concatenationContributors = {};
				for _, expression in ipairs(expressions) do
					for _, readDependency in ipairs(expression.reads) do
						table.insert(readDependencies, readDependency);
					end;
					for _, writeExpression in ipairs(expression.writes) do
						table.insert(writeDependencies, writeExpression);
					end;
					for contributor in pairs(expression.contributors) do
						concatenationContributors[contributor] = true;
					end;
				end;
				return setValue(concatenationContext, {
					t = "concatenation", 
					reads = readDependencies, 
					writes = writeDependencies, 
					contributors = concatenationContributors, 
					precedence = 0, 
					exprs = expressions
				});
			end;
			local function createGetTableExpression(unknownValue, tableExpression, tableIndexData)
				local expressionCreator = setValue;
				local context = unknownValue;
				local getTableExpression = {
					t = "get table", 
					reads = tableAppendFunction(tableExpression.reads, tableIndexData.reads), 
					writes = tableAppendFunction(tableExpression.writes, tableIndexData.writes)
				};
				local leftTableContributors = tableExpression.contributors;
				local indexContributors = tableIndexData.contributors;
				local combinedContributors = table.clone(leftTableContributors);
				for contributorIndex in pairs(indexContributors) do
					combinedContributors[contributorIndex] = true;
				end;
				getTableExpression.contributors = combinedContributors;
				getTableExpression.table = tableExpression;
				getTableExpression.index = tableIndexData;
				return expressionCreator(context, getTableExpression);
			end;
			local function processConstant(condition, constantValue)
				local rotatedBits = bit32.rrotate(bit32.band(constantValue, 3221225472), 30);
				local constant1 = constantsList[bit32.rrotate(bit32.band(constantValue, 1072693248), 20)];
				if rotatedBits == 2 then
					local constant2 = constantsList[bit32.rrotate(bit32.band(constantValue, 1047552), 10)];
					if condition then
						return createConstantIndex(condition, createGlobal(nil, constant1), constant2);
					else
						return createConstantIndex(nil, createGlobal(nil, constant1), constant2);
					end;
				elseif rotatedBits == 3 then
					local constantA = constantsList[bit32.rrotate(bit32.band(constantValue, 1047552), 10)];
					local constantB = constantsList[bit32.band(constantValue, 1023)];
					if condition then
						return createConstantIndex(condition, createConstantIndex(nil, createGlobal(nil, constant1), constantA), constantB);
					else
						return createConstantIndex(nil, createConstantIndex(nil, createGlobal(nil, constant1), constantA), constantB);
					end;
				elseif condition then
					return createGlobal(condition, constant1);
				else
					return createGlobal(nil, constant1);
				end;
			end;
			local function evaluateExpression(startIndex, loopEnd, _, condition)
				local argumentExpressions = {};
				local evaluatedExpression;
				if variableMap[startIndex] then
					evaluatedExpression = setValueWithConversion(startIndex, variableMap[startIndex]);
				else
					local cachedExpression = stack[startIndex];
					if not cachedExpression then
						addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(startIndex) .. "]");
						cachedExpression = setValue(startIndex, (createDefaultMetadata(true)));
					end;
					evaluatedExpression = cachedExpression;
				end;
				if expressionReuseCache[evaluatedExpression] then
					addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
				else
					expressionReuseCache[evaluatedExpression] = true;
				end;
				local baseExpression = evaluatedExpression;
				evaluatedExpression = table.clone(baseExpression.reads);
				local _ = table.clone(baseExpression.writes);
				local _ = table.clone(baseExpression.contributors);
				local loopStart = condition and 2 or 1;
				for indexOffset = loopStart, loopEnd do
					local currentIndex = startIndex + indexOffset;
					local currentExpression;
					if variableMap[currentIndex] then
						currentExpression = setValueWithConversion(currentIndex, variableMap[currentIndex]);
					else
						local currentCachedExpression = stack[currentIndex];
						if not currentCachedExpression then
							addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(currentIndex) .. "]");
							currentCachedExpression = setValue(currentIndex, (createDefaultMetadata(true)));
						end;
						currentExpression = currentCachedExpression;
					end;
					if expressionReuseCache[currentExpression] then
						addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
					else
						expressionReuseCache[currentExpression] = true;
					end;
					table.insert(argumentExpressions, currentExpression);
				end;
				local concatenatedExpression = combineMetadata(baseExpression, argumentExpressions, condition and true or false);
				return setValue(startIndex, concatenatedExpression);
			end;
			local visitedExpressionsMap = {};
			local function _(trueIndex)
				booleanMap[trueIndex] = true;
				visitedExpressionsMap[trueIndex] = true;
			end;
			local function _(nilIndex)
				booleanMap[nilIndex] = nil;
				visitedExpressionsMap[nilIndex] = nil;
			end;
			local function createFunctionInfo(expressionStart, functionData)
				local functionContributors = {};
				local functionName = nil;
				local isNameKnown = nil;
				if functionData.debug_name and settings.use_proto_debugnames then
					functionName = functionData.debug_name;
					isNameKnown = true;
				else
					local tempVariableName = "var" .. tostring(variableCounter);
					local existingVariableCount = variableIndexMap[tempVariableName];
					local uniqueVariableName = tempVariableName;
					local variableCounter = existingVariableCount or 1;
					while variableMap[uniqueVariableName] or globalNameCache[uniqueVariableName] do
						variableCounter = variableCounter + 1;
						uniqueVariableName = tempVariableName .. "_" .. variableCounter;
					end;
					variableIndexMap[tempVariableName] = variableCounter;
					local functionName = uniqueVariableName;
					variableCounter = variableCounter + 1;
					functionName = functionName;
				end;
				local expression = allocateVariable;
				local cachedVariableName = functionName;
				local existingVariableName = variableIndexMap[cachedVariableName];
				local uniqueCachedVariableName = cachedVariableName;
				local isSelfReferencing = existingVariableName or 1;
				while variableMap[uniqueCachedVariableName] or globalNameCache[uniqueCachedVariableName] do
					isSelfReferencing = isSelfReferencing + 1;
					uniqueCachedVariableName = cachedVariableName .. "_" .. isSelfReferencing;
				end;
				variableIndexMap[cachedVariableName] = isSelfReferencing;
				expression = expression(uniqueCachedVariableName, {
					beginning = expressionStart, 
					ending = expressionStart
				}, {});
				local functionInfo = setValue(expressionStart, {
					t = "function", 
					reads = {}, 
					writes = {}, 
					contributors = functionContributors, 
					varname = expression
				});
				cachedVariableName = linesList[#linesList];
				processVariable(expressionStart, functionInfo);
				if not variableMap[expressionStart] then
					assignValuesToRange({
						beginning = expressionStart, 
						ending = expressionStart
					}, functionInfo, {
						expression
					});
				end;
				existingVariableName = linesList[#linesList];
				uniqueCachedVariableName = #linesList;
				success = true;
				isSelfReferencing = false;
				local upvalueList = {};
				for instructionIndex = instructionNumber + 1, #instructionList do
					local instructionData = instructionList[instructionIndex];
					local instruction = instructionData.inst;
					if instructionData.opinfo.opname == "CAPTURE" then
						local captureType = bit32.band(bit32.rshift(instruction, 8), 255);
						if captureType == 2 then
							if parentFunction then
								local upvalueInfo = parentFunction.upvalues[bit32.band(bit32.rshift(instruction, 16), 255)];
								assert(upvalueInfo.name.attributes.is_upvalue);
								table.insert(upvalueList, {
									name = upvalueInfo.name, 
									access = upvalueInfo.access == "readonly" and "copied, readonly" or upvalueInfo.access == "read and write" and "copied, read and write" or upvalueInfo.access
								});
							else
								local unknownUpvalueName = "UNK" .. math.random(1, 1000000000);
								addWarningComment(prefixWarning .. ": Malformed upref, replacing with `" .. unknownUpvalueName .. "`");
								table.insert(upvalueList, {
									name = allocateVariable(unknownUpvalueName, {
										beginning = -1, 
										ending = -1
									}, {}), 
									access = "copied, unknown"
								});
							end;
						elseif not (captureType ~= 0) or captureType == 1 then
							local upvalueIndex = bit32.band(bit32.rshift(instruction, 16), 255);
							if upvalueIndex == expressionStart then
								isSelfReferencing = true;
								local evaluatedExpression;
								if variableMap[expressionStart] then
									evaluatedExpression = setValueWithConversion(expressionStart, variableMap[expressionStart]);
								else
									local defaultExpressionValue = stack[expressionStart];
									if not defaultExpressionValue then
										addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(expressionStart) .. "]");
										defaultExpressionValue = setValue(expressionStart, (createDefaultMetadata(true)));
									end;
									evaluatedExpression = defaultExpressionValue;
								end;
								if evaluatedExpression.t ~= "name" then
									local processExpression = assignValuesToRange;
									local expressionValue = {
										beginning = expressionStart, 
										ending = expressionStart
									};
									local cachedFunctionInfo = functionInfo;
									local arguments = {};
									local range = {
										beginning = expressionStart, 
										ending = expressionStart
									};
									local localTable2 = {};
									local anonymousFunction5 = allocateVariable;
									local variableName = "var" .. tostring(variableCounter);
									local existingValue = variableIndexMap[variableName];
									local uniqueVariableName = variableName;
									local counter = existingValue or 1;
									while variableMap[uniqueVariableName] or globalNameCache[uniqueVariableName] do
										counter = counter + 1;
										uniqueVariableName = variableName .. "_" .. counter;
									end;
									variableIndexMap[variableName] = counter;
									local uniqueVariableNameCopy = uniqueVariableName;
									variableCounter = variableCounter + 1;
									local resultValue = anonymousFunction5(uniqueVariableNameCopy, range, localTable2);
									arrayCopyFunction(arguments, 1, resultValue);
									processExpression(expressionValue, cachedFunctionInfo, arguments);
									if variableMap[expressionStart] then
										evaluatedExpression = setValueWithConversion(expressionStart, variableMap[expressionStart]);
									else
										expressionValue = stack[expressionStart];
										if not expressionValue then
											addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(expressionStart) .. "]");
											expressionValue = setValue(expressionStart, (createDefaultMetadata(true)));
										end;
										evaluatedExpression = expressionValue;
									end;
									processExpression = tempTable[instructionIndex];
									if processExpression then
										for _ = #processExpression, 1, -1 do
											error("message");
										end;
									end;
								end;
								booleanMap[expressionStart] = true;
								visitedExpressionsMap[expressionStart] = true;
								local objectName = evaluatedExpression.name;
								if captureType == 0 then
									if not objectName.attributes.is_upvalue then
										objectName.attributes.is_upvalue = "read";
									end;
									table.insert(upvalueList, {
										name = objectName, 
										access = "readonly"
									});
								else
									objectName.attributes.is_upvalue = "write";
									table.insert(upvalueList, {
										name = objectName, 
										access = "read and write"
									});
								end;
							else
								local evaluatedExpression;
								if variableMap[upvalueIndex] then
									evaluatedExpression = setValueWithConversion(upvalueIndex, variableMap[upvalueIndex]);
								else
									local defaultValue = stack[upvalueIndex];
									if not defaultValue then
										addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(upvalueIndex) .. "]");
										defaultValue = setValue(upvalueIndex, (createDefaultMetadata(true)));
									end;
									evaluatedExpression = defaultValue;
								end;
								if evaluatedExpression.t ~= "name" then
									local anonymousFunction0 = assignValuesToRange2;
									local expressionValue = {
										beginning = upvalueIndex, 
										ending = upvalueIndex
									};
									local resultsTable = {};
									local range = {
										beginning = upvalueIndex, 
										ending = upvalueIndex
									};
									local localTable3 = {};
									local anonymousFunction6 = allocateVariable;
									local variableName2 = "var" .. tostring(variableCounter);
									local existingValue2 = variableIndexMap[variableName2];
									local uniqueVariableName2 = variableName2;
									local counter2 = existingValue2 or 1;
									while variableMap[uniqueVariableName2] or globalNameCache[uniqueVariableName2] do
										counter2 = counter2 + 1;
										uniqueVariableName2 = variableName2 .. "_" .. counter2;
									end;
									variableIndexMap[variableName2] = counter2;
									local uniqueVariableNameCopy2 = uniqueVariableName2;
									variableCounter = variableCounter + 1;
									local resultValue2 = anonymousFunction6(uniqueVariableNameCopy2, range, localTable3);
									arrayCopyFunction(resultsTable, 1, resultValue2);
									anonymousFunction0(expressionValue, resultsTable);
									if variableMap[upvalueIndex] then
										evaluatedExpression = setValueWithConversion(upvalueIndex, variableMap[upvalueIndex]);
									else
										expressionValue = stack[upvalueIndex];
										if not expressionValue then
											addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(upvalueIndex) .. "]");
											expressionValue = setValue(upvalueIndex, (createDefaultMetadata(true)));
										end;
										evaluatedExpression = expressionValue;
									end;
								end;
								booleanMap[upvalueIndex] = true;
								visitedExpressionsMap[upvalueIndex] = true;
								if evaluatedExpression.name.init_expr then
									functionContributors[evaluatedExpression.name.init_expr] = true;
								end;
								if captureType == 0 then
									if not evaluatedExpression.name.attributes.is_upvalue then
										evaluatedExpression.name.attributes.is_upvalue = "read";
									end;
									table.insert(upvalueList, {
										name = evaluatedExpression.name, 
										access = "readonly"
									});
								else
									evaluatedExpression.name.attributes.is_upvalue = "write";
									table.insert(upvalueList, {
										name = evaluatedExpression.name, 
										access = "read and write"
									});
								end;
							end;
						else
							error((("Unknown LCT %*"):format(captureType)));
						end;
					else
						instructionNumber = instructionIndex - 1;
						break;
					end;
				end;
				if #upvalueList > 0 then
					table.move(upvalueList, 1, #upvalueList, 0, upvalueList);
					table.remove(upvalueList, #upvalueList);
				end;
				if cachedVariableName ~= existingVariableName then
					local listCopy = linesList;
					local removedItem = table.remove(linesList, uniqueCachedVariableName);
					assert(removedItem);
					table.insert(listCopy, removedItem);
				end;
				local lines = linesList;
				linesList = {};
				local functionArguments = {};
				for argumentIndex = 0, functionData.params_count - 1 do
					local anonymousFunction7 = allocateVariable;
					local argumentName = expression.name;
					local argumentNameWithPrefix = "arg" .. tostring(argumentCounter);
					if settings.exact_argument_names then
						argumentNameWithPrefix = argumentName .. "_" .. argumentNameWithPrefix;
					end;
					local argumentNameWithPrefixCopy = argumentNameWithPrefix;
					local existingValue3 = variableIndexMap[argumentNameWithPrefixCopy];
					local argumentNameWithPrefixUnique = argumentNameWithPrefixCopy;
					local counter3 = existingValue3 or 1;
					while variableMap[argumentNameWithPrefixUnique] or globalNameCache[argumentNameWithPrefixUnique] do
						counter3 = counter3 + 1;
						argumentNameWithPrefixUnique = argumentNameWithPrefixCopy .. "_" .. counter3;
					end;
					variableIndexMap[argumentNameWithPrefixCopy] = counter3;
					argumentNameWithPrefix = argumentNameWithPrefixUnique;
					argumentCounter = argumentCounter + 1;
					anonymousFunction7 = anonymousFunction7(argumentNameWithPrefix, {
						beginning = argumentIndex, 
						ending = argumentIndex
					}, functionArguments);
					anonymousFunction7.func_name = expression;
					local _ = anonymousFunction7;
				end;
				argumentCounter = 1;
				local ast = protectedCall;
				local functionData = {
					t = "function", 
					reads = {}, 
					writes = {}, 
					contributors = functionContributors, 
					is_self_referencing = isSelfReferencing, 
					name = functionName, 
					varname = expression, 
					name_known = isNameKnown, 
					args = functionArguments, 
					is_vararg = functionData.is_vararg, 
					line_defined = functionData.line_defined, 
					upvalues_count = functionData.upvalues_count, 
					upvalues = upvalueList, 
					ast = ast(functionData, {
						args = functionArguments, 
						upvalues = upvalueList, 
						name = functionName
					})
				};
				table.clear(functionInfo);
				for key, value in pairs(functionData) do
					functionInfo[key] = value;
				end;
				for _, argument in ipairs(functionArguments) do
					local argumentName2 = argument.name;
					if not variableMap[argumentName2] then
						error((("[free] Variable %* not allocated"):format(argument.name)));
					end;
					variableMap[argumentName2] = nil;
				end;
				expression.init_expr = functionInfo;
				expression.var_num = 1;
				linesList = lines;
				return functionInfo;
			end;
			if codeLength == 0 and not codeList[0] then
				addWarningComment((("%*: Empty proto"):format(prefixWarning)));
				unknownValue = localValue;
				return linesList;
			else
				if parentFunction and parentFunction.args then
					for index, argumentValue in ipairs(parentFunction.args) do
						variableMap[index - 1] = argumentValue;
					end;
				end;
				local function _(tableIndex)
					local instruction = opcodeMapByCode[tableIndex];
					if not instruction then
						instructionIndex = instructionIndex + 1;
						return;
					elseif instruction.aux then
						instructionIndex = instructionIndex + 2;
						return;
					else
						instructionIndex = instructionIndex + 1;
						return;
					end;
				end;
				local function _()
					local auxiliaryData = codeList[instructionIndex + 1];
					if not auxiliaryData then
						error("Expected aux");
					end;
					return auxiliaryData;
				end;
				local function _(conditionalStatement)
					local conditionExpression = conditionalStatement.condition;
					assert(conditionExpression);
					conditionalStatement.condition = comparisonOperatorMap[conditionExpression];
				end;
				local function _(conditionalStatement)
					local conditionValue = conditionalStatement.condition;
					local leftHandSide = conditionalStatement.lhs;
					local rightHandSide = conditionalStatement.rhs;
					assert(conditionValue);
					assert(leftHandSide);
					assert(rightHandSide);
					conditionalStatement.lhs = rightHandSide;
					conditionalStatement.rhs = leftHandSide;
					conditionalStatement.condition = conditionOpposites[conditionValue];
				end;
				local destinationMap = {};
				local jumpTable = {};
				local _ = function(key, _, value)
					jumpTable[key] = value;
				end;
				local function processJumpCondition(jumpInfo)
					if jumpInfo.condition then
						local instructionCode = jumpInfo.code;
						local operationName = instructionCode.opname;
						if not (operationName ~= "JUMPXEQKNIL" and operationName ~= "JUMPXEQKB" and operationName ~= "JUMPXEQKN") or operationName == "JUMPXEQKS" then
							local rightHandSide = jumpInfo.rhs;
							assert(rightHandSide);
							return bit32.band(bit32.rshift(instructionCode.inst, 8), 255), rightHandSide;
						elseif not (operationName ~= "JUMPIFEQ" and operationName ~= "JUMPIFLE" and operationName ~= "JUMPIFLT" and operationName ~= "JUMPIFNOTEQ" and operationName ~= "JUMPIFNOTLE") or operationName == "JUMPIFNOTLT" then
							local auxiliaryValue = instructionCode.aux;
							assert(auxiliaryValue);
							local _ = jumpInfo.rhs;
							return bit32.band(bit32.rshift(instructionCode.inst, 8), 255), auxiliaryValue;
						elseif not (operationName ~= "JUMPIF") or operationName == "JUMPIFNOT" then
							return (bit32.band(bit32.rshift(instructionCode.inst, 8), 255));
						elseif not (operationName ~= "JUMP" and operationName ~= "JUMPBACK") or operationName == "JUMPX" then
							return;
						else
							error((("Unknown visitor %*"):format((tostring(operationName)))));
							return;
						end;
					else
						return;
					end;
				end;
				benchmark:end_benchmark("Initialization");
				benchmark:start_benchmark("Initial Processing");
				local instructionIndex = 1;
				local actualCodeIndex = 0;
				local codeIndexToActualIndexMap = {};
				local indexMap = {};
				local function _(instructionData)
					table.insert(instructionList, instructionData);
					codeIndexToActualIndexMap[instructionData.code_index] = instructionData.actual_code_index;
					indexMap[instructionData.index] = instructionData.actual_index;
				end;
				while instructionIndex <= codeLength do
					local bytecodeInstruction = codeList[instructionIndex];
					local opcode = bit32.band(bytecodeInstruction, 255);
					instructionNumber = #instructionList + 1;
					local opcodeInfo = opcodeMapByCode[opcode];
					if not opcodeInfo then
						local errorMessage = ("Unknown opcode 0x%* (%* reversed: 0x%*)"):format(hexLookupTable[bit32.band(bytecodeInstruction, 255)], opcodeEncodingType, hexLookupTable[opcodeReverseMap[bit32.band(bytecodeInstruction, 255)]]);
						error(errorMessage);
						globalFailedInstructionsCount = globalFailedInstructionsCount + 1;
						local opcodeInfoAux = opcodeMapByCode[opcode];
						if not opcodeInfoAux then
							instructionIndex = instructionIndex + 1;
						elseif opcodeInfoAux.aux then
							instructionIndex = instructionIndex + 2;
						else
							instructionIndex = instructionIndex + 1;
						end;
					else
						local opname = opcodeInfo.opname;
						local instruction = {
							index = instructionNumber, 
							code_index = instructionIndex, 
							actual_index = instructionIndex, 
							actual_code_index = actualCodeIndex, 
							opinfo = opcodeInfo, 
							opname = opname, 
							opcode = opcode, 
							inst = bytecodeInstruction, 
							aux = if opcodeInfo.aux then codeList[instructionIndex + 1] else nil, 
							size = opcodeInfo.aux and 2 or 1
						};
						table.insert(instructionList, instruction);
						codeIndexToActualIndexMap[instruction.code_index] = instruction.actual_code_index;
						indexMap[instruction.index] = instruction.actual_index;
						codeIndexToInstructionNumberMap[instructionIndex] = instructionNumber;
						instructionNumberToCodeIndexMap[instructionNumber] = instructionIndex;
						if opcodeUsedMap[opcode] then
							if not (opname ~= "JUMPXEQKNIL" and opname ~= "JUMPXEQKB" and opname ~= "JUMPXEQKN") or opname == "JUMPXEQKS" then
								buffer.writeu16(buffer8, 0, (bit32.band(bit32.rshift(bytecodeInstruction, 16), 65535)));
								local destinationOffset = buffer.readi16(buffer8, 0);
								local auxValue = codeList[instructionIndex + 1];
								if not auxValue then
									error("Expected aux");
								end;
								local jumpOffset = auxValue;
								auxValue = nil;
								auxValue = if opname == "JUMPXEQKNIL" then setValue(nil, (createDefaultMetadata(true))) else if opname == "JUMPXEQKB" then createBoolean(nil, bit32.band(jumpOffset, 1) > 0) else createConstant(nil, constantsList[bit32.band(jumpOffset, 16777215)]);
								local isNotEqualCondition = false;
								if bit32.band(jumpOffset, 2147483648) > 0 then
									isNotEqualCondition = true;
								end;
								jumpTable[instructionNumber] = {
									code = instruction, 
									index = instructionNumber, 
									destination = instructionIndex + destinationOffset + 1, 
									condition = isNotEqualCondition and "~=" or "==", 
									lhs = nil, 
									rhs = auxValue
								};
							elseif not (opname ~= "JUMPIFEQ" and opname ~= "JUMPIFLE" and opname ~= "JUMPIFLT" and opname ~= "JUMPIFNOTEQ" and opname ~= "JUMPIFNOTLE") or opname == "JUMPIFNOTLT" then
								buffer.writeu16(buffer8, 0, (bit32.band(bit32.rshift(bytecodeInstruction, 16), 65535)));
								local destinationOffset2 = buffer.readi16(buffer8, 0);
								local comparisonOperator = nil;
								local operationSuffix = string.sub(opname, #opname - 1, #opname);
								comparisonOperator = if string.match(opname, "NOT") then operationSuffix == "EQ" and "~=" or operationSuffix == "LE" and ">" or ">=" else operationSuffix == "EQ" and "==" or operationSuffix == "LE" and "<=" or "<";
								assert(comparisonOperator, opname);
								jumpTable[instructionNumber] = {
									code = instruction, 
									index = instructionNumber, 
									destination = instructionIndex + destinationOffset2 + 1, 
									condition = comparisonOperator
								};
							elseif not (opname ~= "JUMPIF") or opname == "JUMPIFNOT" then
								buffer.writeu16(buffer8, 0, (bit32.band(bit32.rshift(bytecodeInstruction, 16), 65535)));
								jumpTable[instructionNumber] = {
									code = instruction, 
									index = instructionNumber, 
									destination = instructionIndex + buffer.readi16(buffer8, 0) + 1, 
									condition = opname == "JUMPIFNOT" and "not exist" or "exist"
								};
							elseif not (opname ~= "JUMP" and opname ~= "JUMPBACK") or opname == "JUMPX" then
								local destinationOffset3;
								if opname == "JUMPX" then
									buffer.writeu32(buffer8, 0, (bit32.rshift(bytecodeInstruction, 8)));
									destinationOffset3 = bit32.rshift(buffer.readi32(buffer8, 1), 16);
								else
									buffer.writeu16(buffer8, 0, (bit32.band(bit32.rshift(bytecodeInstruction, 16), 65535)));
									destinationOffset3 = buffer.readi16(buffer8, 0);
								end;
								jumpTable[instructionNumber] = {
									code = instruction, 
									index = instructionNumber, 
									destination = instructionIndex + destinationOffset3 + 1
								};
							elseif opname == "FORNLOOP" then
								local registerIndex = bit32.band(bit32.rshift(bytecodeInstruction, 8), 255);
								buffer.writeu16(buffer8, 0, (bit32.band(bit32.rshift(bytecodeInstruction, 16), 65535)));
								local numericForLoopInfo = {
									code = instruction, 
									index = instructionNumber, 
									destination = instructionIndex + buffer.readi16(buffer8, 0) + 1
								};
								local forLoopData = {
									type = "numeric", 
									variable_count = 1
								};
								local registerRangeBeginning = registerIndex + 2;
								forLoopData.variables_reg_range = {
									beginning = registerRangeBeginning, 
									ending = registerRangeBeginning
								};
								local forLoopArguments = {};
								local registerRangeEnding = registerIndex + 2;
								forLoopArguments.register_range = if registerRangeEnding then {
									beginning = registerIndex, 
									ending = registerRangeEnding
								} else {
										beginning = registerIndex, 
										ending = registerIndex
									};
								forLoopArguments.index_reg = registerIndex + 2;
								forLoopArguments.end_reg = registerIndex;
								forLoopArguments.step_reg = registerIndex + 1;
								forLoopData.args = forLoopArguments;
								numericForLoopInfo.for_info = forLoopData;
								jumpTable[instructionNumber] = numericForLoopInfo;
							elseif opname == "FORGLOOP" then
								local registerIndexGeneric = bit32.band(bit32.rshift(bytecodeInstruction, 8), 255);
								buffer.writeu16(buffer8, 0, (bit32.band(bit32.rshift(bytecodeInstruction, 16), 65535)));
								local destinationOffset4 = buffer.readi16(buffer8, 0);
								local variableCount = codeList[instructionIndex + 1];
								if not variableCount then
									error("Expected aux");
								end;
								variableCount = bit32.band(variableCount, 255);
								assert(variableCount > 0);
								local genericForLoopInfo = {
									code = instruction, 
									index = instructionNumber, 
									destination = instructionIndex + destinationOffset4 + 1
								};
								local genericForLoopData = {
									type = "generic", 
									variable_count = variableCount
								};
								local variablesBeginningRegister = registerIndexGeneric + 3;
								local variablesEndingRegister = registerIndexGeneric + 2 + variableCount;
								genericForLoopData.variables_reg_range = if variablesEndingRegister then {
									beginning = variablesBeginningRegister, 
									ending = variablesEndingRegister
								} else {
										beginning = variablesBeginningRegister, 
										ending = variablesBeginningRegister
									};
								local genericForLoopArguments = {};
								variablesEndingRegister = registerIndexGeneric + 2;
								genericForLoopArguments.register_range = if variablesEndingRegister then {
									beginning = registerIndexGeneric, 
									ending = variablesEndingRegister
								} else {
										beginning = registerIndexGeneric, 
										ending = registerIndexGeneric
									};
								genericForLoopArguments.generator_reg = registerIndexGeneric;
								genericForLoopArguments.state_reg = registerIndexGeneric + 1;
								genericForLoopArguments.index_reg = registerIndexGeneric + 2;
								genericForLoopData.args = genericForLoopArguments;
								genericForLoopData.code = instruction;
								genericForLoopInfo.for_info = genericForLoopData;
								jumpTable[instructionNumber] = genericForLoopInfo;
							elseif opname == "LOADB" then
								local jumpOffset = bit32.band(bit32.rshift(bytecodeInstruction, 24), 255);
								if jumpOffset ~= 0 then
									local jumpInstruction = {
										index = instructionNumber, 
										code_index = instructionIndex, 
										actual_index = instructionIndex, 
										actual_code_index = actualCodeIndex
									};
									local jumpOperationInfo = opcodeMapByName.JUMP;
									if not jumpOperationInfo then
										error((("Unknown opname %*"):format("JUMP")));
									end;
									jumpInstruction.opinfo = jumpOperationInfo;
									jumpInstruction.opname = "JUMP";
									local jumpOpcode = opcodeMapByName.JUMP;
									if not jumpOpcode then
										error((("Unknown opname %*"):format("JUMP")));
									end;
									jumpInstruction.opcode = jumpOpcode.opcode;
									local jumpOpcode = opcodeMapByName.JUMP;
									if not jumpOpcode then
										error((("Unknown opname %*"):format("JUMP")));
									end;
									jumpInstruction.inst = bit32.bor(jumpOpcode.opcode, (bit32.lshift(jumpOffset, 16)));
									jumpInstruction.size = 1;
									table.insert(instructionList, jumpInstruction);
									instructionNumber = instructionNumber + 1;
									instructionNumberToCodeIndexMap[instructionNumber] = instructionIndex;
									jumpTable[instructionNumber] = {
										code = jumpInstruction, 
										index = instructionNumber, 
										destination = instructionIndex + jumpOffset + 1
									};
								end;
							end;
						end;
						instructionIndex = instructionIndex + 1;
						actualCodeIndex = actualCodeIndex + instruction.size;
						local opcodeInfoAux2 = opcodeMapByCode[opcode];
						if not opcodeInfoAux2 then
							instructionIndex = instructionIndex + 1;
						elseif opcodeInfoAux2.aux then
							instructionIndex = instructionIndex + 2;
						else
							instructionIndex = instructionIndex + 1;
						end;
					end;
				end;
				benchmark:end_benchmark("Initial Processing");
				benchmark:start_benchmark("Initial Labeling");
				for _, jump in pairs(jumpTable) do
					codeIndexMap[jump.code.code_index] = createNothingObject();
					assert(jump.destination);
					jump.destination = codeIndexToInstructionNumberMap[jump.destination];
					assert(jump.destination, "Broken control flow");
					local destinationTableEntry = destinationMap[jump.destination];
					if destinationTableEntry then
						table.insert(destinationTableEntry, jump);
					else
						destinationMap[jump.destination] = {
							jump
						};
					end;
				end;
				benchmark:end_benchmark("Initial Labeling");
				benchmark:start_benchmark("Block Loading");
				instructionNumber = 0;
				instructionIndex = 0;
				local jumpedToByMap = {};
				local jumpedToByListMap = {};
				local conditionMet = false;
				local controlFlowEntries = {};
				local instructionInstances = {};
				local instructionIndex = 1;
				for instructionIndexIterator, instruction in ipairs(instructionList) do
					local instructionVisitor = jumpTable[instructionIndexIterator];
					if instructionVisitor then
						if instructionInstances[1] and instructionInstances[1] == instruction then
							table.remove(instructionInstances, 1);
							error("Shouldn't happen");
						end;
						local instructionParents = destinationMap[instructionIndexIterator];
						if instructionParents and instructionIndex ~= instructionIndexIterator then
							assert(#instructionParents > 0);
							assert(instructionIndex < instructionIndexIterator);
							table.insert(controlFlowEntries, {
								insts = instructionInstances, 
								index = instructionIndex
							});
							table.insert(controlFlowEntries, {
								insts = {}, 
								index = instructionIndexIterator, 
								visitor = instructionVisitor
							});
						else
							table.insert(controlFlowEntries, {
								insts = instructionInstances, 
								index = instructionIndex, 
								visitor = instructionVisitor
							});
						end;
						instructionIndex = instructionIndexIterator + 1;
						instructionInstances = {};
					elseif destinationMap[instructionIndexIterator] then
						local hasVisitor = false;
						if #instructionInstances == 0 then
							hasVisitor = true;
							table.insert(instructionInstances, instruction);
						end;
						table.insert(controlFlowEntries, {
							insts = instructionInstances, 
							index = instructionIndex
						});
						if instructionIndex == instructionIndexIterator then
							if hasVisitor then
								instructionIndex = instructionIndexIterator + 1;
								instructionInstances = {};
							else
								instructionIndex = instructionIndexIterator + 1;
								instructionInstances = {
									instruction
								};
							end;
						else
							instructionIndex = instructionIndexIterator;
							instructionInstances = {
								instruction
							};
						end;
					else
						table.insert(instructionInstances, instruction);
					end;
				end;
				if instructionIndex <= #instructionList then
					table.insert(controlFlowEntries, {
						insts = instructionInstances, 
						index = instructionIndex
					});
				end;
				local instructionDataMap = {};
				local instructionList = {};
				local visitorDestinationMap = {};
				for _, controlFlowEntry in ipairs(controlFlowEntries) do
					local visitor = controlFlowEntry.visitor;
					local condition = nil;
					if visitor then
						condition = visitor.condition;
						visitorDestinationMap[controlFlowEntry] = visitor.destination;
					end;
					local instructionData = {
						insts = controlFlowEntry.insts, 
						index = controlFlowEntry.index, 
						visitor = visitor, 
						children = {}
					};
					instructionDataMap[controlFlowEntry.index] = instructionData;
					table.insert(instructionList, instructionData);
				end;
				for controlFlowEntryIndex, controlFlowEntryData in ipairs(controlFlowEntries) do
					local visitorDestination = visitorDestinationMap[controlFlowEntryData];
					if visitorDestination then
						assert(controlFlowEntryData.visitor);
						local destinationInstructionData = instructionDataMap[visitorDestination];
						assert(destinationInstructionData);
						local currentInstructionData = instructionDataMap[controlFlowEntryData.index];
						local children = currentInstructionData.children;
						table.insert(children, destinationInstructionData);
						if controlFlowEntryData.visitor.condition then
							local nextControlFlowEntry = controlFlowEntries[controlFlowEntryIndex + 1];
							if nextControlFlowEntry then
								local nextInstructionData = instructionDataMap[nextControlFlowEntry.index];
								if nextInstructionData ~= currentInstructionData and nextInstructionData ~= destinationInstructionData then
									table.insert(children, nextInstructionData);
								end;
							end;
						end;
					else
						local nextControlFlowEntryConditional = controlFlowEntries[controlFlowEntryIndex + 1];
						if nextControlFlowEntryConditional then
							table.insert(instructionDataMap[controlFlowEntryData.index].children, instructionDataMap[nextControlFlowEntryConditional.index]);
						end;
					end;
				end;
				local instructionDepthMap = {};
				local instructionCloneList = table.clone(jumpTable);
				local instructionMap = table.clone(jumpTable);
				local instructionCount = {};
				local function removeRedundantJumps()

				end;
				local jumpConditionsMet = instructionCount --[[ copy: 109 -> 170 ]];
				local function processJumpCondition(condition)
					local conditionType = condition.type;
					if conditionType == "ifthen" then
						if jumpTable[condition.data.pass].jump then
							jumpConditionsMet[condition] = true;
							return;
						end;
					elseif conditionType == "ifthenelse" then
						local elseCondition = condition.data.else_;
						assert(elseCondition);
						if jumpTable[elseCondition].jump then
							jumpConditionsMet[condition] = true;
							return;
						end;
					end;
					jumpConditionsMet[condition] = nil;
				end;
				local function addParentToJumpedToBy(parentInstruction, instruction)
					if jumpedToByMap[instruction.index] then
						local jumpedToByList = jumpedToByListMap[instruction.index];
						if jumpedToByList then
							if not table.find(jumpedToByList, parentInstruction) then
								table.insert(jumpedToByList, parentInstruction);
								return;
							else
								error("TEMPORARY thing");
								return;
							end;
						else
							jumpedToByListMap[instruction.index] = {
								parentInstruction
							};
						end;
					end;
				end;
				local instructionList = instructionCloneList --[[ copy: 107 -> 171 ]];
				local instructionMap = instructionDepthMap --[[ copy: 106 -> 172 ]];
				local addParentToJumpedToByLocal = addParentToJumpedToBy --[[ copy: 112 -> 173 ]];
				local function addInstruction(instructionData, instructionList)
					table.insert(jumpTable, instructionData);
					table.insert(instructionList, instructionData);
					if instructionData.index > 0 then
						instructionMap[instructionData.index] = instructionData;
					end;
					if instructionList then
						for _, childInstruction in ipairs(instructionList) do
							addParentToJumpedToByLocal(instructionData, childInstruction);
						end;
					end;
				end;
				local copiedFunction = removeRedundantJumps --[[ copy: 110 -> 174 ]];
				local function calculateEndIndex(instructionA, instructionB)
					local highLevelIndex = instructionA.hl_index;
					assert(highLevelIndex == instructionB.hl_index);
					local instructionIndex = instructionA.index;
					assert(instructionIndex == instructionB.index);
					jumpTable[highLevelIndex] = instructionB;
					instructionList[highLevelIndex] = instructionB;
					if instructionIndex > 0 then
						assert(instructionMap[instructionIndex]);
						instructionMap[instructionIndex] = instructionB;
					end;
					copiedFunction();
				end;
				for _, instructionIterator in ipairs(instructionList) do
					assert(instructionIterator.index == instructionDataMap[instructionIterator.index].index);
					local instructions = instructionDataMap[instructionIterator.index].insts;
					local instruction = {
						code_index = instructionNumberToCodeIndexMap[instructionIterator.index], 
						index = instructionIterator.index, 
						actual_code_index = codeIndexToActualIndexMap[instructionNumberToCodeIndexMap[instructionIterator.index]], 
						actual_index = indexMap[instructionIterator.index], 
						hl_index = #jumpTable + 1, 
						length = if instructionIterator.visitor then #instructions + 1 else #instructions, 
						type = "single", 
						data = instructions, 
						parents = {}, 
						children = {}, 
						jumped_to_by = {}, 
						_visitor = instructionIterator.visitor
					};
					addInstruction(instruction);
					table.insert(instructionMap, instruction);
					jumpedToByMap[instructionIterator.index] = instruction;
				end;
				local callbackFunction = addInstruction --[[ copy: 113 -> 175 ]];
				local function createEmptyInstruction()
					local emptyInstruction = {
						code_index = 0, 
						index = 0, 
						actual_code_index = 0, 
						actual_index = 0, 
						hl_index = #jumpTable + 1, 
						length = 0, 
						type = "single", 
						data = {}, 
						parents = {}, 
						children = {}, 
						jumped_to_by = {}
					};
					callbackFunction(emptyInstruction);
					return emptyInstruction;
				end;
				local function handleGotoJump(currentInstruction, destinationInstructionIndex)
					assert(not currentInstruction.jump);
					currentInstruction.jump = {
						type = "goto", 
						destination = destinationInstructionIndex
					};
					local destinationInstruction = jumpTable[destinationInstructionIndex];
					assert(destinationInstruction);
					table.insert(jumpedToByMap[destinationInstruction.index].jumped_to_by, currentInstruction.hl_index);
				end;
				local function checkRegisterWritesInRange(jumpData)
					local jumpData = jumpData.jump;
					assert(jumpData);
					local destinationData = jumpTable[jumpData.destination];
					assert(destinationData);
					local highlightIndex = jumpData.hl_index;
					local jumpedToByList = jumpedToByMap[destinationData.index].jumped_to_by;
					local jumpedToIndex = table.find(jumpedToByList, highlightIndex);
					assert(jumpedToIndex);
					table.remove(jumpedToByList, jumpedToIndex);
				end;
				local function removeJump(jumpContext)
					local jumpDataLocal = jumpContext.jump;
					assert(jumpDataLocal);
					local destinationDataLocal = jumpTable[jumpDataLocal.destination];
					assert(destinationDataLocal);
					local highlightIndexLocal = jumpContext.hl_index;
					local jumpedToByListLocal = jumpedToByMap[destinationDataLocal.index].jumped_to_by;
					local jumpedToIndex1 = table.find(jumpedToByListLocal, highlightIndexLocal);
					assert(jumpedToIndex1);
					table.remove(jumpedToByListLocal, jumpedToIndex1);
					jumpContext.jump = nil;
				end;
				local function defineVariable(gotoContext, destinationIndex)
					local jumpDataDestination = gotoContext.jump;
					assert(jumpDataDestination);
					local destinationDataDestination = jumpTable[jumpDataDestination.destination];
					assert(destinationDataDestination);
					local highlightIndexDestination = gotoContext.hl_index;
					local jumpedToByListDestination = jumpedToByMap[destinationDataDestination.index].jumped_to_by;
					local jumpedToIndex2 = table.find(jumpedToByListDestination, highlightIndexDestination);
					assert(jumpedToIndex2);
					table.remove(jumpedToByListDestination, jumpedToIndex2);
					gotoContext.jump = nil;
					assert(not gotoContext.jump);
					gotoContext.jump = {
						type = "goto", 
						destination = destinationIndex
					};
					jumpDataDestination = jumpTable[destinationIndex];
					assert(jumpDataDestination);
					table.insert(jumpedToByMap[jumpDataDestination.index].jumped_to_by, gotoContext.hl_index);
				end;
				local createContext = createEmptyInstruction --[[ copy: 115 -> 176 ]];
				local function createJumpContext(destination)
					local newContext = createContext();
					assert(not newContext.jump);
					newContext.jump = {
						type = "goto", 
						destination = destination
					};
					local destinationDataNew = jumpTable[destination];
					assert(destinationDataNew);
					table.insert(jumpedToByMap[destinationDataNew.index].jumped_to_by, newContext.hl_index);
					return newContext;
				end;
				local function addPreprocessTask()
					local newBreakJump = createContext();
					newBreakJump.jump = {
						type = "break", 
						destination = 0
					};
					return newBreakJump;
				end;
				local function isChild(parentInstruction, childInstruction)
					if table.find(parentInstruction.children, childInstruction.hl_index) then
						return true;
					else
						return false;
					end;
				end;
				local function isUnlockable(parentContext, instructionToCheck)
					if table.find(parentContext.children, instructionToCheck.hl_index) and true or false then
						if not (instructionToCheck.index == parentContext.index + parentContext.length) or instructionToCheck.index == 0 then
							return true;
						else
							local visitor = parentContext._visitor;
							if visitor and visitor.condition then
								return true;
							end;
						end;
					end;
				end;
				local function addChild(parent, child)
					assert(parent);
					assert(child);
					assert(not table.find(jumpTable[child.hl_index].parents, parent.hl_index));
					assert(not table.find(jumpTable[parent.hl_index].children, child.hl_index));
					table.insert(parent.children, child.hl_index);
					table.insert(child.parents, parent.hl_index);
				end;
				local function removeChild(parentInstance, childInstance)
					assert(parentInstance);
					assert(childInstance);
					local childIndex = table.find(parentInstance.children, childInstance.hl_index);
					assert(childIndex);
					local parentIndex = table.find(childInstance.parents, parentInstance.hl_index);
					assert(parentIndex);
					table.remove(parentInstance.children, childIndex);
					table.remove(childInstance.parents, parentIndex);
				end;
				local callbackFunctionCopy = removeChild --[[ copy: 125 -> 177 ]];
				local function defineVariable(instance)
					for _, childIndex in ipairs(table.clone(instance.children)) do
						callbackFunctionCopy(instance, jumpTable[childIndex]);
					end;
				end;
				local function clearVariableRange(instanceWithParents)
					for _, parentIndex in ipairs(table.clone(instanceWithParents.parents)) do
						callbackFunctionCopy(jumpTable[parentIndex], instanceWithParents);
					end;
				end;
				local clearVariableRangeCopy = clearVariableRange --[[ copy: 127 -> 178 ]];
				local callbackFunction2 = defineVariable --[[ copy: 126 -> 179 ]];
				local function addRegisterRead(instanceToProcess)
					clearVariableRangeCopy(instanceToProcess);
					callbackFunction2(instanceToProcess);
				end;
				local function moveHierarchy(oldParent, oldChild, parentInstruction2)
					assert(oldParent);
					assert(oldChild);
					assert(parentInstruction2);
					local childIndexInParent = table.find(oldParent.children, oldChild.hl_index);
					assert(childIndexInParent);
					local parentIndex2 = table.find(oldChild.parents, oldParent.hl_index);
					assert(parentIndex2);
					assert(not table.find(parentInstruction2.parents, oldParent.hl_index));
					table.remove(oldChild.parents, parentIndex2);
					table.insert(parentInstruction2.parents, oldParent.hl_index);
					oldParent.children[childIndexInParent] = parentInstruction2.hl_index;
				end;
				local function processedRegisters(oldParentInstance, oldChildInstance, parentInstruction3)
					assert(oldParentInstance);
					assert(oldChildInstance);
					assert(parentInstruction3);
					local parentIndexInParent = table.find(oldParentInstance.parents, oldChildInstance.hl_index);
					assert(parentIndexInParent);
					local childIndex2 = table.find(oldChildInstance.children, oldParentInstance.hl_index);
					assert(childIndex2);
					assert(not table.find(parentInstruction3.children, oldParentInstance.hl_index));
					table.remove(oldChildInstance.children, childIndex2);
					table.insert(parentInstruction3.children, oldParentInstance.hl_index);
					oldParentInstance.parents[parentIndexInParent] = parentInstruction3.hl_index;
				end;
				local destinationTableCopy = instructionDepthMap --[[ copy: 106 -> 180 ]];
				local function processDemands(codeBlock, hasChildren)
					local visitor = codeBlock._visitor;
					if visitor and visitor.destination then
						assert(table.find(codeBlock.children, destinationTableCopy[visitor.destination].hl_index));
					end;
					if hasChildren then
						local childIndices = table.create(#codeBlock.children);
						for _, childIndexValue in ipairs(codeBlock.children) do
							if jumpTable[childIndexValue].index > codeBlock.index then
								table.insert(childIndices, childIndexValue);
							end;
						end;
						return childIndices;
					else
						return (table.clone(codeBlock.children));
					end;
				end;
				local processDemandsCopy = processDemands --[[ copy: 131 -> 181 ]];
				local function createRegisterSnapshot(codeBlockToProcess)
					local childList = processDemandsCopy(codeBlockToProcess);
					if codeBlockToProcess.type == "ifthen" then
						table.insert(childList, codeBlockToProcess.data.pass);
						return childList;
					elseif codeBlockToProcess.type == "ifthenelse" then
						table.insert(childList, codeBlockToProcess.data.pass);
						table.insert(childList, codeBlockToProcess.data.else_);
						return childList;
					elseif codeBlockToProcess.type == "oneblockwhile" then
						table.insert(childList, codeBlockToProcess.data.code);
						return childList;
					else
						if codeBlockToProcess.type == "sequential" then
							for _, instructionData in ipairs(codeBlockToProcess.data) do
								table.insert(childList, instructionData);
							end;
						end;
						return childList;
					end;
				end;
				local function isAncestor(potentialAncestor, targetAncestor, visitedAncestors, parent)
					if potentialAncestor == targetAncestor then
						return true;
					else
						visitedAncestors = visitedAncestors or {};
						assert(visitedAncestors);
						for _, parentIndex in ipairs((processDemandsCopy(potentialAncestor, parent))) do
							if not visitedAncestors[parentIndex] then
								visitedAncestors[parentIndex] = true;
								if table.find(targetAncestor.parents, parentIndex) then
									return true;
								else
									return isAncestor(jumpTable[parentIndex], targetAncestor, visitedAncestors);
								end;
							end;
						end;
						return false;
					end;
				end;
				local getParentIndices = createRegisterSnapshot --[[ copy: 132 -> 182 ]];
				local function propagateVariable(variableA, variableB, visitedVariables)
					if variableA == variableB then
						return true;
					else
						visitedVariables = visitedVariables or {};
						assert(visitedVariables);
						for _, childIndex in ipairs((getParentIndices(variableA))) do
							if not visitedVariables[childIndex] then
								visitedVariables[childIndex] = true;
								if table.find(variableB.parents, childIndex) then
									return true;
								else
									return propagateVariable(jumpTable[childIndex], variableB, visitedVariables);
								end;
							end;
						end;
						return false;
					end;
				end;
				local propagateParent = moveHierarchy --[[ copy: 129 -> 183 ]];
				local function cycleCount(parentA, instructionB)
					assert(parentA ~= instructionB);
					assert(parentA.hl_index ~= instructionB.hl_index);
					local parentList = table.clone(parentA.parents);
					for indexInList in ipairs(parentList) do
						local parentIndex = parentList[indexInList];
						propagateParent(jumpTable[parentIndex], parentA, instructionB);
					end;
				end;
				local propagateChild = processedRegisters --[[ copy: 130 -> 184 ]];
				local function childCount(childA, instructionD)
					assert(childA ~= instructionD);
					assert(childA.hl_index ~= instructionD.hl_index);
					local childrenList = table.clone(childA.children);
					for _, childIndex in ipairs(childrenList) do
						propagateChild(jumpTable[childIndex], childA, instructionD);
					end;
				end;
				local removeParent = cycleCount --[[ copy: 135 -> 185 ]];
				local removeChild = childCount --[[ copy: 136 -> 186 ]];
				local function variableCount(variableA, variableB)
					removeParent(variableA, variableB);
					removeChild(variableA, variableB);
				end;
				for _, variableData in ipairs(instructionList) do
					local variableInfo = instructionDepthMap[variableData.index];
					assert(variableData);
					for _, childInstructionIndex in ipairs(variableData.children) do
						addChild(variableInfo, instructionDepthMap[childInstructionIndex.index]);
					end;
					table.clear(variableData.children);
				end;
				local function locationVariables(conditionType, conditionValue, conditionLhs, rightHandSide, codeBlock)
					return {
						is_full_condition_t = true, 
						type = conditionType, 
						condition = conditionValue, 
						lhs = conditionLhs, 
						rhs = rightHandSide, 
						code = codeBlock
					};
				end;
				local function analyzeCondition(leftHandSide, conditionRhsAnd, conditionExists)
					return {
						is_full_condition_t = true, 
						type = "and", 
						condition = conditionExists and "not exist" or "exist", 
						lhs = leftHandSide, 
						rhs = conditionRhsAnd, 
						code = nil
					};
				end;
				local function createCondition(leftHandSideOr, conditionRhsOr, conditionExistsOr)
					return {
						is_full_condition_t = true, 
						type = "or", 
						condition = conditionExistsOr and "not exist" or "exist", 
						lhs = leftHandSideOr, 
						rhs = conditionRhsOr, 
						code = nil
					};
				end;
				local function createVisitor(visitor1, visitor2)
					local isParentValid = false;
					local isParentValid2 = false;
					local closestParent = nil;
					for _, parentIndex in ipairs(visitor1.parents) do
						if table.find(visitor2.parents, parentIndex) then
							local parentVisitor = jumpTable[parentIndex];
							local isValid;
							if table.find(parentVisitor.children, visitor2.hl_index) and true or false then
								if not (visitor2.index == parentVisitor.index + parentVisitor.length) or visitor2.index == 0 then
									isValid = true;
									isParentValid = true;
								else
									local visitorCondition2 = parentVisitor._visitor;
									if visitorCondition2 and visitorCondition2.condition then
										isValid = true;
										isParentValid = true;
									end;
								end;
							end;
							if not isParentValid then
								if not isParentValid then
									isValid = nil;
								end;
							end;
							isParentValid = false;
							if isValid then
								if closestParent then
									if jumpTable[parentIndex].index > jumpTable[closestParent].index then
										closestParent = parentIndex;
									end;
								else
									closestParent = parentIndex;
								end;
							end;
						end;
					end;
					local conditionMet = nil;
					if closestParent then
						conditionMet = false;
					else
						conditionMet = true;
						for _, parentIndex2 in ipairs(visitor2.parents) do
							if table.find(visitor1.parents, parentIndex2) then
								local parentVisitor2 = jumpTable[parentIndex2];
								local isValid2;
								if table.find(parentVisitor2.children, visitor1.hl_index) and true or false then
									if not (visitor1.index == parentVisitor2.index + parentVisitor2.length) or visitor1.index == 0 then
										isValid2 = true;
										isParentValid2 = true;
									else
										local visitorCondition3 = parentVisitor2._visitor;
										if visitorCondition3 and visitorCondition3.condition then
											isValid2 = true;
											isParentValid2 = true;
										end;
									end;
								end;
								if not isParentValid2 then
									if not isParentValid2 then
										isValid2 = nil;
									end;
								end;
								isParentValid2 = false;
								if isValid2 then
									assert(not closestParent);
									closestParent = parentIndex2;
								end;
							end;
						end;
					end;
					return jumpTable[closestParent], conditionMet;
				end;
				local processTask = removeRedundantJumps --[[ copy: 110 -> 187 ]];
				local visitorCheck = createVisitor --[[ copy: 141 -> 188 ]];
				local processChild = removeChild --[[ copy: 125 -> 189 ]];
				local function processVisitors(visitorA, visitorB, conditionCheck)
					local isChildValid = false;
					processTask();
					local currentVisitor, hasSibling = visitorCheck(visitorA, visitorB);
					assert(currentVisitor);
					local visitorStack = {
						currentVisitor
					};
					local alternativeCondition = nil;
					local lastElement = visitorStack[#visitorStack];
					local totalValue = 0;
					local visitedNodesMap = {};
					while true do
						if conditionStopPointsMap[currentVisitor.index] then
							lastElement = visitorStack[#visitorStack];
							break;
						elseif #currentVisitor.parents ~= 1 then
							lastElement = visitorStack[#visitorStack];
							if #currentVisitor.parents ~= 0 then
								local currentNode = visitorStack[#visitorStack];
								if visitorCheck(currentNode, visitorA) then
									local successFlag, value1, table1, returnVariable4 = processVisitors(currentNode, visitorA, true);
									alternativeCondition = successFlag;
									lastElement = value1;
									totalValue = totalValue + returnVariable4;
									for indexInTable in pairs(table1) do
										visitedNodesMap[indexInTable] = true;
									end;
									break;
								elseif visitorCheck(currentNode, visitorB) then
									local successFlag2, value2, table2, returnVariable8 = processVisitors(currentNode, visitorB);
									alternativeCondition = successFlag2;
									lastElement = value2;
									totalValue = totalValue + returnVariable8;
									for indexInTable2 in pairs(table2) do
										visitedNodesMap[indexInTable2] = true;
									end;
									break;
								else
									break;
								end;
							else
								break;
							end;
						else
							local parentNode = jumpTable[currentVisitor.parents[1]];
							if #parentNode.children < 2 then
								lastElement = visitorStack[#visitorStack];
								break;
							else
								local firstChildIndex = parentNode.children[1];
								if firstChildIndex ~= visitorA.hl_index and firstChildIndex ~= visitorB.hl_index then
									lastElement = visitorStack[#visitorStack];
									break;
								else
									currentVisitor = parentNode;
									table.insert(visitorStack, currentVisitor);
									visitedNodesMap[currentVisitor] = true;
								end;
							end;
						end;
					end;
					local conditionTree = nil;
					local currentInstruction = nil;
					for index, element in ipairs(visitorStack) do
						assert(element._visitor);
						assert(element._visitor.condition);
						local conditionMet;
						if table.find(element.children, visitorB.hl_index) and true or false then
							if not (visitorB.index == element.index + element.length) or visitorB.index == 0 then
								conditionMet = true;
								isChildValid = true;
							else
								local visitorCondition = element._visitor;
								if visitorCondition and visitorCondition.condition then
									conditionMet = true;
									isChildValid = true;
								end;
							end;
						end;
						if not isChildValid then
							if not isChildValid then
								conditionMet = nil;
							end;
						end;
						isChildValid = false;
						local isFirstElement = nil;
						isFirstElement = not conditionMet and index == 1;
						if conditionCheck then
							conditionMet = not conditionMet;
						end;
						if hasSibling and index == 1 then
							isFirstElement = not isFirstElement;
						end;
						local lhs, conditionRhs = processJumpCondition(element._visitor);
						if isFirstElement or conditionMet then
							local visitor = element._visitor;
							local conditionValue = visitor.condition;
							assert(conditionValue);
							visitor.condition = comparisonOperatorMap[conditionValue];
						end;
						if conditionTree then
							local conditionNode = {
								is_full_condition_t = true, 
								type = "reg", 
								condition = element._visitor.condition, 
								lhs = lhs, 
								rhs = conditionRhs, 
								code = element.hl_index
							};
							if conditionMet then
								conditionTree = {
									is_full_condition_t = true, 
									type = "and", 
									condition = isFirstElement and "not exist" or "exist", 
									lhs = conditionNode, 
									rhs = conditionTree, 
									code = nil
								};
							else
								conditionTree = {
									is_full_condition_t = true, 
									type = "or", 
									condition = isFirstElement and "not exist" or "exist", 
									lhs = conditionNode, 
									rhs = conditionTree, 
									code = nil
								};
							end;
						else
							conditionTree = {
								is_full_condition_t = true, 
								type = "reg", 
								condition = element._visitor.condition, 
								lhs = lhs, 
								rhs = conditionRhs, 
								code = element.hl_index
							};
						end;
						totalValue = totalValue + element.length;
						if currentInstruction then
							processChild(element, currentInstruction);
							processChild(element, jumpTable[element.children[1]]);
						end;
						currentInstruction = element;
					end;
					if alternativeCondition then
						conditionTree = {
							is_full_condition_t = true, 
							type = "or", 
							condition = "exist", 
							lhs = alternativeCondition, 
							rhs = conditionTree, 
							code = nil
						};
					end;
					local firstElement = visitorStack[1];
					processChild(firstElement, visitorA);
					if visitorA.hl_index ~= visitorB.hl_index then
						processChild(firstElement, visitorB);
					end;
					processTask();
					return conditionTree, lastElement, visitedNodesMap, totalValue;
				end;
				local foundFlag = false;
				benchmark:end_benchmark("Block Loading");
				benchmark:start_benchmark("Control Flow Analysis");
				local function processNode(node, breakConditions, continueStatements)
					local jumpData = node.jump;
					if jumpData then
						local destinationIndex = jumpTable[jumpData.destination].index;
						if breakConditions[destinationIndex] then
							jumpData.type = "break";
							local jumpData2 = node.jump;
							assert(jumpData2);
							local destinationNode = jumpTable[jumpData2.destination];
							assert(destinationNode);
							local nodeHlIndex = node.hl_index;
							local jumpedToBy = jumpedToByMap[destinationNode.index].jumped_to_by;
							local jumpIndex = table.find(jumpedToBy, nodeHlIndex);
							assert(jumpIndex);
							table.remove(jumpedToBy, jumpIndex);
						elseif continueStatements[destinationIndex] then
							jumpData.type = "continue";
							local jumpData3 = node.jump;
							assert(jumpData3);
							local bytecodeInstruction = jumpTable[jumpData3.destination];
							assert(bytecodeInstruction);
							local highlightIndex = node.hl_index;
							local jumpedToByList = jumpedToByMap[bytecodeInstruction.index].jumped_to_by;
							local jumpIndex2 = table.find(jumpedToByList, highlightIndex);
							assert(jumpIndex2);
							table.remove(jumpedToByList, jumpIndex2);
						end;
					end;
					local instructionType = node.type;
					if instructionType == "sequential" then
						for _, dataValue in ipairs(node.data) do
							processNode(jumpTable[dataValue], breakConditions, continueStatements);
						end;
						return;
					else
						if not (instructionType ~= "ifthen") or instructionType == "ifthenelse" then
							local instructionData = node.data;
							processNode(jumpTable[instructionData.pass], breakConditions, continueStatements);
							local elseInstruction = instructionData.else_;
							if elseInstruction then
								processNode(jumpTable[elseInstruction], breakConditions, continueStatements);
							end;
						end;
						return;
					end;
				end;
				local neglectedInstructions = nil;
				local bytecodeBlockListCopy = instructionCloneList --[[ copy: 107 -> 190 ]];
				local preprocessTaskIndexCopy = instructionCount --[[ copy: 109 -> 191 ]];
				local unknownFunction1 = addParentToJumpedToBy --[[ copy: 112 -> 192 ]];
				local unknownFunction2 = childCount --[[ copy: 136 -> 193 ]];
				local unknownFunction3 = addInstruction --[[ copy: 113 -> 194 ]];
				local unknownFunction4 = cycleCount --[[ copy: 135 -> 195 ]];
				do
					local neglectedInstructionCount = neglectedInstructions;
					local function optimizeBytecodeBlocks()
						local optimizationOccurred = false;
						while true do
							local optimizationAttemptCount = 0;
							for bytecodeBlockIndex = #bytecodeBlockListCopy, 1, -1 do
								local bytecodeBlock = jumpTable[bytecodeBlockIndex];
								if #bytecodeBlock.children == 1 then
									local childBytecodeBlock = jumpTable[bytecodeBlock.children[1]];
									if childBytecodeBlock ~= bytecodeBlock and not (table.find(childBytecodeBlock.children, childBytecodeBlock.hl_index) and true or false) and #childBytecodeBlock.parents <= 1 and #childBytecodeBlock.jumped_to_by <= 0 and childBytecodeBlock.index >= bytecodeBlock.index then
										local sequentialBytecodeBlock = if bytecodeBlock.type == "sequential" then jumpTable[bytecodeBlock.data[#bytecodeBlock.data]] else bytecodeBlock;
										if preprocessTaskIndexCopy[sequentialBytecodeBlock] then
											local ifthenData = sequentialBytecodeBlock.data;
											local passBytecodeBlock = jumpTable[ifthenData.pass];
											if passBytecodeBlock then
												local jumpInfo = passBytecodeBlock.jump;
												assert(jumpInfo);
												if not (not (jumpInfo.type == "goto") or passBytecodeBlock.index == 0) then
													continue;
												end;
											end;
											if ifthenData.else_ and jumpTable[ifthenData.else_] then
												error("IDK");
											end;
										end;
										if table.find(bytecodeBlock.children, childBytecodeBlock.hl_index) and true or false then
											if not (childBytecodeBlock.index == bytecodeBlock.index + bytecodeBlock.length) or childBytecodeBlock.index == 0 then
												sequentialBytecodeBlock = true;
												optimizationOccurred = true;
											else
												local visitorCondition2 = bytecodeBlock._visitor;
												if visitorCondition2 and visitorCondition2.condition then
													sequentialBytecodeBlock = true;
													optimizationOccurred = true;
												end;
											end;
										end;
										if not optimizationOccurred then
											if not optimizationOccurred then
												sequentialBytecodeBlock = nil;
											end;
										end;
										optimizationOccurred = false;
										if sequentialBytecodeBlock then

										end;
										assert(table.find(childBytecodeBlock.parents, bytecodeBlock.hl_index));
										if bytecodeBlock.type == "sequential" then
											if not bytecodeBlock._visitor or not bytecodeBlock._visitor.condition then
												neglectedInstructionCount = neglectedInstructionCount + 1;
												optimizationAttemptCount = optimizationAttemptCount + 1;
												table.insert(bytecodeBlock.data, childBytecodeBlock.hl_index);
												bytecodeBlock.length = bytecodeBlock.length + childBytecodeBlock.length;
												unknownFunction1(bytecodeBlock, childBytecodeBlock);
												processChild(bytecodeBlock, childBytecodeBlock);
												unknownFunction2(childBytecodeBlock, bytecodeBlock);
												bytecodeBlock._visitor = childBytecodeBlock._visitor;
											else
												continue;
											end;
										elseif not bytecodeBlock._visitor or not bytecodeBlock._visitor.condition then
											neglectedInstructionCount = neglectedInstructionCount + 1;
											optimizationAttemptCount = optimizationAttemptCount + 1;
											sequentialBytecodeBlock = #jumpTable + 1;
											local sequentialInstruction = {
												code_index = instructionNumberToCodeIndexMap[bytecodeBlock.index], 
												index = bytecodeBlock.index, 
												actual_code_index = codeIndexToActualIndexMap[instructionNumberToCodeIndexMap[bytecodeBlock.index]], 
												actual_index = indexMap[bytecodeBlock.index], 
												hl_index = sequentialBytecodeBlock, 
												length = bytecodeBlock.length + childBytecodeBlock.length, 
												type = "sequential", 
												data = {
													bytecodeBlock.hl_index, 
													childBytecodeBlock.hl_index
												}, 
												parents = {}, 
												children = {}, 
												jumped_to_by = {}, 
												_visitor = childBytecodeBlock._visitor
											};
											unknownFunction3(sequentialInstruction, {
												bytecodeBlock, 
												childBytecodeBlock
											});
											unknownFunction4(bytecodeBlock, sequentialInstruction);
											processChild(bytecodeBlock, childBytecodeBlock);
											unknownFunction2(childBytecodeBlock, sequentialInstruction);
										else
											continue;
										end;
										processTask();
									end;
								end;
							end;
							if not (optimizationAttemptCount ~= 0) then
								break;
							end;
						end;
					end;
					while true do
						neglectedInstructionCount = 0;
						removeRedundantJumps();
						for bytecodeBlockIndex2 = #instructionCloneList, 1, -1 do
							local bytecodeBlock2 = jumpTable[bytecodeBlockIndex2];
							if not (#bytecodeBlock2.children ~= 1) or #bytecodeBlock2.children == 2 then
								local conditionFlag = false;
								local visitorCondition3 = bytecodeBlock2._visitor;
								if visitorCondition3 and visitorCondition3.condition then
									assert(isUnlockable(bytecodeBlock2, jumpTable[bytecodeBlock2.children[1]]));
									local firstChild = bytecodeBlock2.children[1];
									assert(firstChild);
									if #jumpTable[firstChild].children <= 1 then
										local secondChild = bytecodeBlock2.children[2];
										local firstVisitedChild = nil;
										local currentBlock = nil;
										local nextBlock = nil;
										if secondChild then
											assert(secondChild);
											assert(firstChild ~= secondChild);
											if #jumpTable[secondChild].children <= 1 then
												local children = jumpTable[firstChild].children;
												if #children <= 1 then
													local _ = children[1];
													local firstChildIndex = jumpTable[secondChild].children[1];
													local isFirstChildValid = firstChildIndex and isUnlockable(jumpTable[secondChild], jumpTable[firstChildIndex]);
													if isFirstChildValid then
														if foundFlag then
															nextBlock = jumpTable[firstChildIndex];
														else
															continue;
														end;
													end;
													firstVisitedChild = jumpTable[firstChild];
													currentBlock = jumpTable[secondChild];
													if firstVisitedChild ~= bytecodeBlock2 then
														if isFirstChildValid then
															if not (#firstVisitedChild.parents >= 1) then
																continue;
															end;
														elseif not (#firstVisitedChild.parents >= 2) then
															continue;
														end;
														if not currentBlock or not firstVisitedChild then
															error("Shouldn't happen...");
														end;
														if isFirstChildValid or isChild(currentBlock, firstVisitedChild) then
															local nextSibling = instructionDepthMap[currentBlock.index + currentBlock.length];
															if isFirstChildValid and nextSibling and nextSibling == firstVisitedChild then
																assert(nextBlock);
																if nextBlock.index <= firstVisitedChild.index and not isAncestor(firstVisitedChild, currentBlock, {}, true) then
																	nextBlock = jumpTable[firstVisitedChild.hl_index];
																	local previousBlock = firstVisitedChild;
																	firstVisitedChild = currentBlock;
																	local currentBlockCopy = currentBlock;
																	currentBlock = createEmptyInstruction();
																	moveHierarchy(bytecodeBlock2, currentBlockCopy, currentBlock);
																	moveHierarchy(bytecodeBlock2, previousBlock, firstVisitedChild);
																	assert(nextBlock);
																	addChild(currentBlock, nextBlock);
																end;
															end;
														else
															continue;
														end;
													else
														continue;
													end;
												else
													continue;
												end;
											else
												continue;
											end;
										else
											firstVisitedChild = jumpTable[firstChild];
											currentBlock = createEmptyInstruction();
											addChild(currentBlock, firstVisitedChild);
											conditionFlag = true;
										end;
										local conditionResult, ifThenData, ifThenParents, conditionLength = processVisitors(if conditionFlag then firstVisitedChild else currentBlock, firstVisitedChild, currentBlock.index == 0);
										local ifThenBlock = {
											code_index = ifThenData.code_index, 
											index = ifThenData.index, 
											actual_code_index = ifThenData.actual_code_index, 
											actual_index = ifThenData.actual_index, 
											hl_index = #jumpTable + 1, 
											length = conditionLength + currentBlock.length, 
											type = "ifthen", 
											data = {
												pass = currentBlock.hl_index, 
												condition = conditionResult, 
												condition_length = conditionLength
											}, 
											parents = {}, 
											children = {}, 
											jumped_to_by = {}
										};
										local blockList = {
											currentBlock
										};
										for index in pairs(ifThenParents) do
											table.insert(blockList, index);
										end;
										addInstruction(ifThenBlock, blockList);
										if nextBlock then
											removeChild(currentBlock, nextBlock);
											handleGotoJump(currentBlock, nextBlock.hl_index);
										else
											removeChild(currentBlock, firstVisitedChild);
										end;
										cycleCount(ifThenData, ifThenBlock);
										addChild(ifThenBlock, firstVisitedChild);
										if isChild(currentBlock, firstVisitedChild) then
											removeChild(bytecodeBlock2, currentBlock);
										end;
										processJumpCondition(ifThenBlock);
										removeRedundantJumps();
									end;
								end;
							end;
						end;
						for block in pairs(table.clone(instructionCount)) do
							if #block.children == 1 then
								removeRedundantJumps();
								local elseBlock = jumpTable[block.children[1]];
								if elseBlock and #elseBlock.children == 1 then
									local blockData = block.data;
									local passBlock = jumpTable[blockData.pass];
									assert(passBlock.jump);
									local jumpDestinationBlock = jumpTable[passBlock.jump.destination];
									assert(jumpDestinationBlock);
									local elseChildBlock = jumpTable[elseBlock.children[1]];
									if jumpDestinationBlock.hl_index == elseChildBlock.hl_index then
										neglectedInstructionCount = neglectedInstructionCount + 1;
										removeRedundantJumps();
										blockData.else_ = elseBlock.hl_index;
										block.type = "ifthenelse";
										block.length = block.length + elseBlock.length;
										removeJump(passBlock);
										instructionCount[block] = nil;
										addChild(block, jumpDestinationBlock);
										removeChild(block, elseBlock);
										removeChild(elseBlock, jumpDestinationBlock);
										processJumpCondition(block);
										removeRedundantJumps();
									end;
								end;
							end;
						end;
						while true do
							local loopCounter = 0;
							for i = #instructionCloneList, 1, -1 do
								local blockAtIndex = jumpTable[i];
								if #blockAtIndex.children == 1 then
									local visitor2 = blockAtIndex._visitor;
									if (not visitor2 or not visitor2.condition) and blockAtIndex.children[1] == blockAtIndex.hl_index and visitor2 then
										assert(visitor2);
										neglectedInstructionCount = neglectedInstructionCount + 1;
										loopCounter = loopCounter + 1;
										local oneBlockWhileBlock = {
											code_index = blockAtIndex.code_index, 
											index = blockAtIndex.index, 
											actual_code_index = blockAtIndex.actual_code_index, 
											actual_index = blockAtIndex.actual_index, 
											hl_index = #jumpTable + 1, 
											length = blockAtIndex.length, 
											type = "oneblockwhile", 
											data = {
												code = blockAtIndex.hl_index, 
												for_info = visitor2.for_info
											}, 
											parents = {}, 
											children = {}, 
											jumped_to_by = {}, 
											_visitor = blockAtIndex._visitor
										};
										addInstruction(oneBlockWhileBlock, {
											blockAtIndex
										});
										variableCount(blockAtIndex, oneBlockWhileBlock);
										removeChild(oneBlockWhileBlock, oneBlockWhileBlock);
										local nextBlockAfterWhile = instructionDepthMap[oneBlockWhileBlock.index + oneBlockWhileBlock.length];
										local visitedIndices = {};
										if nextBlockAfterWhile then
											addChild(oneBlockWhileBlock, nextBlockAfterWhile);
											visitedIndices[nextBlockAfterWhile.index] = true;
										end;
										processNode(blockAtIndex, visitedIndices, {
											[oneBlockWhileBlock.index] = true, 
											[oneBlockWhileBlock.index + oneBlockWhileBlock.length - 1] = true
										});
									end;
								end;
							end;
							if not (loopCounter ~= 0) then
								break;
							end;
						end;
						optimizeBytecodeBlocks();
						if not (neglectedInstructionCount == 0) then
							foundFlag = false;
							continue;
						end;
						if not foundFlag then
							foundFlag = true;
						else
							break;
						end;
					end;
					benchmark:end_benchmark("Control Flow Analysis");
					benchmark:start_benchmark("Control Flow Recovery");
					neglectedInstructionCount = 0;
					while true do
						neglectedInstructionCount = 0;
						for _, parentBlock in ipairs(instructionCloneList) do
							if #parentBlock.parents > 0 then
								local parentListClone = table.clone(parentBlock.parents);
								table.insert(parentListClone, parentBlock.hl_index);
								local _ = parentBlock.index;
								local totalLength = parentBlock.length;
								local startIndex = parentBlock.index;
								for _, parentIndex in ipairs(parentBlock.parents) do
									local parentBlockAtIndex = jumpTable[parentIndex];
									if #parentBlockAtIndex.parents <= 0 and #parentBlockAtIndex.children == 1 then
										assert(parentBlockAtIndex.children[1] == parentBlock.hl_index);
										totalLength = totalLength + parentBlockAtIndex.length;
										if parentBlockAtIndex.index < startIndex and parentBlockAtIndex.index > 0 then
											startIndex = parentBlockAtIndex.index;
										end;
										parentBlockAtIndex.analysis_failed = true;
									else
										break;
									end;
								end;
								for _, parentIndexClone in ipairs(table.clone(parentBlock.parents)) do
									removeChild(jumpTable[parentIndexClone], parentBlock);
								end;
								addInstruction({
									code_index = instructionNumberToCodeIndexMap[startIndex], 
									index = startIndex, 
									actual_code_index = codeIndexToActualIndexMap[instructionNumberToCodeIndexMap[startIndex]], 
									actual_index = indexMap[startIndex], 
									hl_index = #jumpTable + 1, 
									length = totalLength, 
									type = "sequential", 
									data = parentListClone, 
									parents = {}, 
									children = parentBlock.children, 
									jumped_to_by = {}
								});
								parentBlock.analysis_failed = true;
								clearVariableRange(parentBlock);
								neglectedInstructionCount = neglectedInstructionCount + 1;
								conditionMet = true;
							end;
						end;
						if not (neglectedInstructionCount ~= 0) then
							break;
						end;
					end;
					removeRedundantJumps();
				end;
				benchmark:end_benchmark("Control Flow Recovery");
				benchmark:start_benchmark("Stack Incantation");
				controlFlowEntries = function()
					for i = #jumpTable, 1, -1 do
						local codeBlock = jumpTable[i];
						if codeBlock.index == 1 then
							if #jumpTable > 1 then

							end;
							return codeBlock;
						end;
					end;
					error("Critical control flow failure: Failed to find entry point");
				end;
				instructionInstances = function(depthLimit, depth)
					assert(depth < depthLimit.depth);
					local parentBlock = depthLimit;
					while parentBlock.depth ~= depth do
						parentBlock = parentBlock.parent;
						assert(parentBlock);
					end;
					return parentBlock;
				end;
				instructionIndex = function(ancestorNode, targetNode)
					if ancestorNode.depth < targetNode.depth then
						return false;
					else
						local ancestorBlock = ancestorNode;
						while true do
							if ancestorBlock.depth ~= targetNode.depth then
								ancestorBlock = ancestorBlock.parent;
								if not ancestorBlock then
									return false;
								end;
							else
								return ancestorBlock.hl_index == targetNode.hl_index;
							end;
						end;
					end;
				end;
				instructionDataMap = {};
				instructionList = {};
				visitorDestinationMap = {};
				instructionDepthMap = function(nodeA, nodeB)
					local commonDepth = nil;
					local depthA = nodeA.depth;
					local depthB = nodeB.depth;
					if depthA == depthB then
						commonDepth = depthA;
					elseif depthA < depthB then
						commonDepth = depthA;
						local localNodeB = nodeB;
						local localCommonDepthA = commonDepth;
						assert(localCommonDepthA < localNodeB.depth);
						local localLocalNodeB = localNodeB;
						while localLocalNodeB.depth ~= localCommonDepthA do
							localLocalNodeB = localLocalNodeB.parent;
							assert(localLocalNodeB);
						end;
						nodeB = localLocalNodeB;
					else
						commonDepth = depthB;
						local localNodeA = nodeA;
						local localCommonDepthB = commonDepth;
						assert(localCommonDepthB < localNodeA.depth);
						local localLocalNodeA = localNodeA;
						while localLocalNodeA.depth ~= localCommonDepthB do
							localLocalNodeA = localLocalNodeA.parent;
							assert(localLocalNodeA);
						end;
						nodeA = localLocalNodeA;
					end;
					assert(nodeA.depth == nodeB.depth);
					while nodeA.hl_index ~= nodeB.hl_index do
						nodeA = nodeA.parent;
						assert(nodeA);
						nodeB = nodeB.parent;
						assert(nodeB);
					end;
					return nodeA;
				end;
				instructionCloneList = {};
				instructionMap = {};
				instructionCount = 0;
				removeRedundantJumps = {};
				processJumpCondition = function(registerTable, registerIndex, maxDepth)
					local globalReadRegisters = registerTable.reg_reads_global;
					local globalWriteRegisters = registerTable.reg_writes_global;
					local cachedValue = nil;
					local returnVariable12 = nil;
					local readRegister = globalReadRegisters[registerIndex];
					local registerValue;
					if readRegister then
						registerValue = readRegister;
					else
						local newRegister = {};
						globalReadRegisters[registerIndex] = newRegister;
						registerValue = newRegister;
					end;
					if #registerValue > 0 then
						for index2 = #registerValue, 1, -1 do
							local existingValue = registerValue[index2];
							if existingValue.location.depth <= maxDepth then
								cachedValue = existingValue;
								break;
							end;
						end;
					end;
					local writeRegister = globalWriteRegisters[registerIndex];
					if writeRegister then
						readRegister = writeRegister;
					else
						local newWriteRegister = {};
						globalWriteRegisters[registerIndex] = newWriteRegister;
						readRegister = newWriteRegister;
					end;
					if #readRegister > 0 then
						for index3 = #readRegister, 1, -1 do
							local valueAtIndex = readRegister[index3];
							if valueAtIndex.location.depth <= maxDepth then
								return cachedValue, valueAtIndex;
							end;
						end;
					end;
					return cachedValue, returnVariable12;
				end;
				addParentToJumpedToBy = function(input1, input2, _)
					local codeLookupResult, codeLookupData = processJumpCondition(input1, input2, index);
					local codeIndex = if codeLookupResult then codeLookupResult.code.index else -1;
					if codeLookupData then
						return codeIndex, codeLookupData.code.index;
					else
						return codeIndex, -1;
					end;
				end;
				addInstruction = function(indexLength)
					return indexLength.index + indexLength.length;
				end;
				calculateEndIndex = function(indexLength2)
					if indexLength2._visitor then
						return indexLength2.index + indexLength2.length - 2;
					else
						return indexLength2.index + indexLength2.length - 1;
					end;
				end;
				createEmptyInstruction = function(locationData, startCode, registerIndex2)
					local registerData = {
						hl_index = locationData.location.hl_index, 
						start_code = startCode, 
						register = registerIndex2
					};
					if locationData.neglected[registerIndex2] then

					end;
					local neglectedRegisters = locationData.neglected;
					local existingNeglected = neglectedRegisters[registerIndex2];
					local neglectedList;
					if existingNeglected then
						neglectedList = existingNeglected;
					else
						local newNeglectedList = {};
						neglectedRegisters[registerIndex2] = newNeglectedList;
						neglectedList = newNeglectedList;
					end;
					table.insert(neglectedList, registerData);
				end;
				handleGotoJump = function(locationData2, _, neglectedIndex)
					local neglectedRegister = locationData2.neglected[neglectedIndex];
					if neglectedRegister then
						for _ = #neglectedRegister, 1, -1 do

						end;
					end;
				end;
				checkRegisterWritesInRange = function(checkData, range)
					local checkType = checkData.type;
					if checkType == "single" then
						local highlightData = instructionCloneList[checkData.hl_index];
						assert(highlightData);
						local registerWrites = highlightData.reg_writes;
						for registerIndex = range.beginning, range.ending do
							if registerWrites[registerIndex] then
								return true;
							end;
						end;
						return false;
					elseif checkType == "sequential" then
						for _, dataValue in ipairs(checkData.data) do
							if checkRegisterWritesInRange(jumpTable[dataValue], range) then
								return true;
							end;
						end;
						return false;
					elseif checkType == "ifthen" then
						assert(not checkData.data.else_);
						return false;
					elseif checkType == "ifthenelse" then
						local dataElse = checkData.data;
						local elseBlock = dataElse.else_;
						assert(elseBlock);
						return checkRegisterWritesInRange(jumpTable[dataElse.pass], range) and checkRegisterWritesInRange(jumpTable[elseBlock], range);
					elseif checkType == "oneblockwhile" then
						local dataCode = checkData.data;
						return checkRegisterWritesInRange(jumpTable[dataCode.code], range);
					else
						error((("Unknown hl_block type %*"):format(checkType)));
						return;
					end;
				end;
				removeJump = function(analysisContext, registerRange, definitionPoint, isLocationParent)
					local isPredefinition = nil;
					local definitionPointIndex = nil;
					local definitionLocation = nil;
					local registerData = nil;
					if definitionPoint then
						assert(isLocationParent);
						registerData = definitionPoint;
					else
						assert(not isLocationParent);
						local _, registerValue = processJumpCondition(analysisContext, registerRange.register_range.beginning, analysisContext.location.depth);
						registerData = registerValue;
					end;
					local isNameCall = not registerData;
					if not isNameCall then
						assert(registerData);
						if registerData.code.opname == "NAMECALL" then
							isNameCall = true;
						else
							local currentLocation = registerData.location;
							local parentLocation = registerRange.location;
							if parentLocation.depth <= currentLocation.depth then
								isNameCall = true;
							else
								while parentLocation.depth ~= currentLocation.depth + 1 do
									parentLocation = parentLocation.parent;
									assert(parentLocation);
								end;
								assert(parentLocation.parent);
								if jumpTable[(if isLocationParent then parentLocation.parent.parent else parentLocation.parent).hl_index].index ~= jumpTable[currentLocation.hl_index].index then
									isNameCall = true;
								else
									local cachedValue = instructionDataMap[parentLocation.hl_index];
									if cachedValue then
										local _ = cachedValue;
									end;
									definitionLocation = registerData.location;
									definitionPointIndex = registerData.code.index;
								end;
							end;
						end;
					end;
					if isNameCall then
						local analysisLocation = analysisContext.location;
						local locationInfo = registerRange.location;
						definitionLocation = instructionDepthMap(analysisLocation, locationInfo);
						local bytecodeInstruction = jumpedToByMap[jumpTable[definitionLocation.hl_index].index];
						definitionPointIndex = if bytecodeInstruction._visitor then bytecodeInstruction.index + bytecodeInstruction.length - 2 else bytecodeInstruction.index + bytecodeInstruction.length - 1;
						isPredefinition = true;
					end;
					assert(definitionPointIndex);
					table.insert(analysisContext.observations, {
						type = "scoped variable", 
						info = {
							wanted_definition_point = definitionPointIndex, 
							definition_location = definitionLocation, 
							is_predefinition = isPredefinition, 
							register_range = registerRange.register_range
						}, 
						location = analysisContext.location
					});
				end;
				defineVariable = function(definitionMap, registerRange, startOfUse, definitionLocationPoint, isPredefinition, noDefinition)
					local variableDefinition = {
						type = "define variable", 
						info = {
							register_range = registerRange, 
							definition_location = definitionLocationPoint, 
							is_predefinition = isPredefinition, 
							no_definition = noDefinition, 
							start_of_use = startOfUse
						}
					};
					local existingDefinitions = definitionMap[startOfUse];
					if existingDefinitions then
						local definitionFound = false;
						for _, existingDefinition in ipairs(existingDefinitions) do
							local newBeginning = registerRange.beginning;
							local existingRegisterRange = existingDefinition.info.register_range;
							local existingBeginning = existingRegisterRange.beginning;
							local existingEnding = existingRegisterRange.ending;
							local isWithinRange = false;
							if existingBeginning <= newBeginning then
								isWithinRange = newBeginning <= existingEnding;
							end;
							if isWithinRange then
								assert(not definitionFound);
								existingDefinition.info.definition_location = definitionLocationPoint;
								definitionFound = true;
								error("messasdasdasdasdasdasdasdasdasdasdage");
							end;
						end;
						if not definitionFound then
							table.insert(existingDefinitions, variableDefinition);
							return;
						end;
					else
						definitionMap[startOfUse] = {
							variableDefinition
						};
					end;
				end;
				createJumpContext = function(highlightIndex, scopeInfo, hasParent, conditionCheck2)
					local loopContext = {
						hl_index = highlightIndex, 
						depth = if hasParent then scopeInfo.depth + 1 else scopeInfo.depth, 
						parent = if hasParent then scopeInfo else scopeInfo.parent, 
						last_loop = scopeInfo.last_loop
					};
					if conditionCheck2 then
						loopContext.last_loop = loopContext;
					end;
					return loopContext;
				end;
				addPreprocessTask = function(preprocessTask, variableIndex, variableType, variableRegisterRange, predefined, noInline)
					instructionCount = instructionCount + 1;
					preprocessTask.preprocess_task_index = instructionCount;
					removeRedundantJumps[instructionCount] = variableIndex;
					local variableData = {
						type = variableType, 
						var_reg_range = variableRegisterRange, 
						predef = predefined, 
						no_inline = noInline
					};
					local variableList = tempTable[variableIndex];
					if variableList then
						for _, variableItem in ipairs(variableList) do
							if variableItem.type == variableType then
								local registerBeginning = variableRegisterRange.beginning;
								local itemRegisterRange = variableItem.var_reg_range;
								local itemRegisterBeginning = itemRegisterRange.beginning;
								local itemRegisterEnding = itemRegisterRange.ending;
								local isWithinRange = false;
								if itemRegisterBeginning <= registerBeginning then
									isWithinRange = registerBeginning <= itemRegisterEnding;
								end;
								if isWithinRange then
									return variableItem;
								end;
							end;
						end;
						table.insert(variableList, variableData);
						return variableData;
					else
						tempTable[variableIndex] = {
							variableData
						};
						return variableData;
					end;
				end;
				isChild = function(variableToLock, variableToLock, lockVariable)
					addPreprocessTask(variableToLock, lockVariable, "lockvar", variableToLock);
				end;
				isUnlockable = function(variableToUnlock, variableToUnlock, unlockVariable)
					addPreprocessTask(variableToUnlock, unlockVariable, "unlockvar", variableToUnlock);
				end;
				addChild = function(registerData, variableDefinition, lockVariable2, _)
					local registerRange = variableDefinition.register_range;
					for registerIndex = registerRange.beginning, registerRange.ending do
						local forcedVariableList = registerData.reg_forced_var[registerIndex];
						if forcedVariableList then
							table.insert(forcedVariableList, variableDefinition);
						else
							registerData.reg_forced_var[registerIndex] = {
								variableDefinition
							};
						end;
						registerData.reg_has_var[registerIndex] = registerRange;
					end;
					addPreprocessTask(registerData, lockVariable2, "lockvar", registerRange);
				end;
				removeChild = function(registerData, registerIndex, unlockVariable3)
					local forcedVariableList = registerData.reg_forced_var[registerIndex];
					if forcedVariableList then
						for index4 = #forcedVariableList, 1, -1 do
							local forcedVariable = forcedVariableList[index4];
							local forcedVariableRegisterRange = forcedVariable.register_range;
							local forcedVariableRegisterBeginning = forcedVariableRegisterRange.beginning;
							local forcedVariableRegisterEnding = forcedVariableRegisterRange.ending;
							local isForcedVariableInRange = false;
							if forcedVariableRegisterBeginning <= registerIndex then
								isForcedVariableInRange = registerIndex <= forcedVariableRegisterEnding;
							end;
							if isForcedVariableInRange then
								if not (registerData.location.depth <= forcedVariable.definition_location.depth) or registerData.location.hl_index == forcedVariable.definition_location.hl_index then
									return forcedVariable;
								else
									isForcedVariableInRange = forcedVariable.register_range;
									addPreprocessTask(registerData, unlockVariable3, "unlockvar", isForcedVariableInRange);
									forcedVariableList[index4] = nil;
								end;
							end;
						end;
						if #forcedVariableList == 0 then
							registerData.reg_forced_var[registerIndex] = nil;
						end;
					end;
					return nil;
				end;
				defineVariable = function(registerState, registerRange, codeIndex, isGlobalWrite, variableDefinition)
					if isGlobalWrite then
						assert(codeIndex);
						addPreprocessTask(registerState, codeIndex, "defvar", registerRange, true, variableDefinition);
					elseif codeIndex then
						addPreprocessTask(registerState, codeIndex, "defvar", registerRange, nil, variableDefinition);
					else
						local lastWrite = registerState.reg_writes_global[registerRange.beginning];
						local lastWriteEntry = lastWrite[#lastWrite];
						addPreprocessTask(registerState, lastWriteEntry.code.code_index, "defvar", registerRange, nil, variableDefinition);
					end;
					for registerIndex = registerRange.beginning, registerRange.ending do
						registerState.reg_has_var[registerIndex] = registerRange;
					end;
				end;
				clearVariableRange = function(registerMap, registerIndex)
					local existingRegister = registerMap.reg_has_var[registerIndex];
					for registerIndex = existingRegister.beginning, existingRegister.ending do
						registerMap.reg_has_var[registerIndex] = nil;
					end;
				end;
				addRegisterRead = function(registerContext, registerIndex, codeBlock)
					local registerReadsGlobal = registerContext.reg_reads;
					local registerReadValue = registerReadsGlobal[registerIndex];
					local registerValue;
					if registerReadValue then
						registerValue = registerReadValue;
					else
						local newRegisterRead = {};
						registerReadsGlobal[registerIndex] = newRegisterRead;
						registerValue = newRegisterRead;
					end;
					table.insert(registerValue, codeBlock);
					registerReadsGlobal = registerContext.reg_reads_global;
					registerReadValue = registerReadsGlobal[registerIndex];
					if registerReadValue then
						registerValue = registerReadValue;
					else
						local newRegisterRead2 = {};
						registerReadsGlobal[registerIndex] = newRegisterRead2;
						registerValue = newRegisterRead2;
					end;
					table.insert(registerValue, codeBlock);
					local _ = codeBlock.code;
					registerValue = registerContext.neglected[registerIndex];
					if registerValue then
						for _ = #registerValue, 1, -1 do

						end;
					end;
				end;
				moveHierarchy = function(registerContext2, registerIndex2, codeData)
					local registerWritesGlobal = registerContext2.reg_writes;
					local registerWriteValue = registerWritesGlobal[registerIndex2];
					local registerWriteInfo;
					if registerWriteValue then
						registerWriteInfo = registerWriteValue;
					else
						local newRegisterWrite = {};
						registerWritesGlobal[registerIndex2] = newRegisterWrite;
						registerWriteInfo = newRegisterWrite;
					end;
					table.insert(registerWriteInfo, codeData);
					registerWritesGlobal = registerContext2.reg_writes_global;
					registerWriteValue = registerWritesGlobal[registerIndex2];
					if registerWriteValue then
						registerWriteInfo = registerWriteValue;
					else
						local newRegisterWrite2 = {};
						registerWritesGlobal[registerIndex2] = newRegisterWrite2;
						registerWriteInfo = newRegisterWrite2;
					end;
					table.insert(registerWriteInfo, codeData);
					registerWriteInfo = {
						hl_index = registerContext2.location.hl_index, 
						start_code = codeData.code, 
						register = registerIndex2
					};
					if registerContext2.neglected[registerIndex2] then

					end;
					local neglectedRegisters = registerContext2.neglected;
					local neglectedRegisterValue = neglectedRegisters[registerIndex2];
					if neglectedRegisterValue then
						registerWriteValue = neglectedRegisterValue;
					else
						local newNeglectedRegister = {};
						neglectedRegisters[registerIndex2] = newNeglectedRegister;
						registerWriteValue = newNeglectedRegister;
					end;
					table.insert(registerWriteValue, registerWriteInfo);
				end;
				processedRegisters = {};
				processDemands = function(registerContext3, registerValue, demandsList, codeInfo)
					local registerHasVariable = registerContext3.reg_has_var;
					if codeInfo and not processedRegisters[demandsList] then
						table.insert(demandsList, codeInfo.code);
						processedRegisters[demandsList] = true;
					end;
					local clonedTable = {};
					for _, demand in ipairs(demandsList) do
						instructionMap[demand.index] = registerValue;
						local forcedDefinitions = {};
						local demandList = registerContext3.demands[demand.index];
						if demandList then
							for index = #demandList, 1, -1 do
								local demandEntry = demandList[index];
								if demandEntry.type == "define variable" then
									local registerRange = demandEntry.info.register_range;
									for registerIndex = registerRange.beginning, registerRange.ending do
										local forcedVariable = registerContext3.reg_forced_var[registerIndex];
										local registerInfo = {
											register_range = registerRange, 
											beginning = demand.index, 
											definition_location = demandEntry.info.definition_location, 
											start_of_use = demandEntry.info.start_of_use
										};
										if forcedVariable then
											table.insert(forcedVariable, registerInfo);
										elseif demandEntry.info.is_predefinition then
											assert(not demandEntry.info.no_definition);
											defineVariable(registerContext3, registerRange, demand.code_index, true, true);
											addChild(registerContext3, registerInfo, demand.code_index);
										elseif demandEntry.info.no_definition then
											addChild(registerContext3, registerInfo, demand.code_index);
										else
											table.insert(forcedDefinitions, registerInfo);
										end;
									end;
								else
									error((("Unknown demand %*"):format(demandEntry.type)));
								end;
							end;
							registerContext3.demands[demand.index] = nil;
						end;
						local registers, registerList, _ = processOp(demand);
						local registerHasVariableMap = {};
						local registerRangeSet = {};
						for _, register in ipairs(registers) do
							registerRangeSet[register] = true;
							local registerVariableInfo = removeChild(registerContext3, register, demand.code_index);
							if registerVariableInfo then
								registerHasVariableMap[register] = registerVariableInfo;
								registerHasVariable[register] = registerVariableInfo.register_range;
							end;
						end;
						for _, registerId in ipairs(registerList) do
							if not registerRangeSet[registerId] then
								local registerInfoById = removeChild(registerContext3, registerId, demand.code_index);
								if registerInfoById then
									registerHasVariableMap[registerId] = registerInfoById;
									registerHasVariable[registerId] = registerInfoById.register_range;
								end;
							end;
						end;
						for key in pairs(clonedTable) do
							if not registerHasVariableMap[key] then

							end;
						end;
						clonedTable = table.clone(registerHasVariableMap);
						registerRangeSet = nil;
						if #registerList > 1 then
							local isConsecutive = true;
							local consecutiveRegister = registerList[1];
							for index = 2, #registerList do
								consecutiveRegister = consecutiveRegister + 1;
								if registerList[index] ~= consecutiveRegister then
									isConsecutive = nil;
									break;
								end;
							end;
							if isConsecutive then
								local firstRegister = registerList[1];
								local lastRegister = registerList[#registerList];
								registerRangeSet = if lastRegister then {
									beginning = firstRegister, 
									ending = lastRegister
								} else {
										beginning = firstRegister, 
										ending = firstRegister
									};
							end;
						elseif #registerList == 1 then
							local firstValue = registerList[1];
							registerRangeSet = {
								beginning = firstValue, 
								ending = firstValue
							};
						end;
						if #registers == 0 then
							for _, registerIndex2 in ipairs(registerList) do
								if registerHasVariable[registerIndex2] and not registerHasVariableMap[registerIndex2] then
									local registerHasVariable = registerContext3.reg_has_var[registerIndex2];
									for registerIndex = registerHasVariable.beginning, registerHasVariable.ending do
										registerContext3.reg_has_var[registerIndex] = nil;
									end;
								end;
								registerContext3.uncertain_regs[registerIndex2] = nil;
								local registerWriteInfo = {
									code = demand, 
									location = registerContext3.location, 
									reg_range = registerRangeSet
								};
								local registerWrites = registerContext3.reg_writes;
								local existingRegisterWrite = registerWrites[registerIndex2];
								local registerWriteList;
								if existingRegisterWrite then
									registerWriteList = existingRegisterWrite;
								else
									local newRegisterWriteList = {};
									registerWrites[registerIndex2] = newRegisterWriteList;
									registerWriteList = newRegisterWriteList;
								end;
								table.insert(registerWriteList, registerWriteInfo);
								registerWrites = registerContext3.reg_writes_global;
								existingRegisterWrite = registerWrites[registerIndex2];
								if existingRegisterWrite then
									registerWriteList = existingRegisterWrite;
								else
									local newRegisterWriteList2 = {};
									registerWrites[registerIndex2] = newRegisterWriteList2;
									registerWriteList = newRegisterWriteList2;
								end;
								table.insert(registerWriteList, registerWriteInfo);
								registerWriteList = {
									hl_index = registerContext3.location.hl_index, 
									start_code = registerWriteInfo.code, 
									register = registerIndex2
								};
								if registerContext3.neglected[registerIndex2] then

								end;
								local neglectedRegisters = registerContext3.neglected;
								local neglectedRegisterWrite = neglectedRegisters[registerIndex2];
								if neglectedRegisterWrite then
									existingRegisterWrite = neglectedRegisterWrite;
								else
									local newNeglectedRegisterWrite = {};
									neglectedRegisters[registerIndex2] = newNeglectedRegisterWrite;
									existingRegisterWrite = newNeglectedRegisterWrite;
								end;
								table.insert(existingRegisterWrite, registerWriteList);
							end;
						else
							for _, registerIndex3 in ipairs(registers) do
								if not registerHasVariableMap[registerIndex3] then
									local uncertainRegister = registerContext3.uncertain_regs[registerIndex3];
									if uncertainRegister then
										local currentLocation = registerContext3.location;
										local currentLocation = uncertainRegister.location;
										local isAncestor;
										if currentLocation.depth < currentLocation.depth then
											isAncestor = false;
										else
											local ancestorLocation = currentLocation;
											while true do
												if ancestorLocation.depth ~= currentLocation.depth then
													ancestorLocation = ancestorLocation.parent;
													if not ancestorLocation then
														isAncestor = false;
														break;
													end;
												else
													isAncestor = ancestorLocation.hl_index == currentLocation.hl_index;
													break;
												end;
											end;
										end;
										if not isAncestor then
											removeJump(registerContext3, uncertainRegister);
										end;
									else
										local globalRegWrite = registerContext3.reg_writes_global[registerIndex3];
										if globalRegWrite and #globalRegWrite > 0 then
											local lastGlobalRegWrite = globalRegWrite[#globalRegWrite];
											local lastLoopLocation = registerContext3.location.last_loop;
											if lastLoopLocation then
												local currentLocation = registerContext3.location;
												local isWithinLastLoop;
												if currentLocation.depth < lastLoopLocation.depth then
													isWithinLastLoop = false;
												else
													local ancestorLocation = currentLocation;
													while true do
														if ancestorLocation.depth ~= lastLoopLocation.depth then
															ancestorLocation = ancestorLocation.parent;
															if not ancestorLocation then
																isWithinLastLoop = false;
																break;
															end;
														else
															isWithinLastLoop = ancestorLocation.hl_index == lastLoopLocation.hl_index;
															break;
														end;
													end;
												end;
												if isWithinLastLoop then
													local currentLocation = lastGlobalRegWrite.location;
													if currentLocation.depth < lastLoopLocation.depth then
														isWithinLastLoop = false;
													else
														local ancestorLocation = currentLocation;
														while true do
															if ancestorLocation.depth ~= lastLoopLocation.depth then
																ancestorLocation = ancestorLocation.parent;
																if not ancestorLocation then
																	isWithinLastLoop = false;
																	break;
																end;
															else
																isWithinLastLoop = ancestorLocation.hl_index == lastLoopLocation.hl_index;
																break;
															end;
														end;
													end;
													if not isWithinLastLoop then
														isWithinLastLoop = {
															index = demand.index, 
															location = currentLocation, 
															register_range = {
																beginning = registerIndex3, 
																ending = registerIndex3
															}
														};
														removeJump(registerContext3, isWithinLastLoop, lastGlobalRegWrite, true);
													end;
												end;
											end;
										end;
									end;
								end;
								if not registerHasVariable[registerIndex3] and not registerHasVariableMap[registerIndex3] then
									local globalRegReads = registerContext3.reg_reads_global;
									local globalRegWrite = globalRegReads[registerIndex3];
									local regWrite;
									if globalRegWrite then
										regWrite = globalRegWrite;
									else
										local newRegWrite = {};
										globalRegReads[registerIndex3] = newRegWrite;
										regWrite = newRegWrite;
									end;
									if #regWrite > 0 then
										globalRegWrite = registerContext3.reg_writes_global;
										local lastGlobalRegRead = globalRegWrite[registerIndex3];
										if lastGlobalRegRead then
											globalRegReads = lastGlobalRegRead;
										else
											local newRegRead = {};
											globalRegWrite[registerIndex3] = newRegRead;
											globalRegReads = newRegRead;
										end;
										globalRegWrite = regWrite[#regWrite];
										lastGlobalRegRead = globalRegReads[#globalRegReads];
										if lastGlobalRegRead and lastGlobalRegRead.code.opname ~= "NAMECALL" and globalRegWrite.code.index > lastGlobalRegRead.code.index then
											defineVariable(registerContext3, lastGlobalRegRead.reg_range, lastGlobalRegRead.code.code_index);
										end;
									end;
								end;
								local regWriteInfo = {
									code = demand, 
									location = registerContext3.location, 
									reg_range = {
										beginning = registerIndex3, 
										ending = registerIndex3
									}
								};
								local regReads = registerContext3.reg_reads;
								local regRead = regReads[registerIndex3];
								local regReadsList;
								if regRead then
									regReadsList = regRead;
								else
									local newRegReadList = {};
									regReads[registerIndex3] = newRegReadList;
									regReadsList = newRegReadList;
								end;
								table.insert(regReadsList, regWriteInfo);
								regReads = registerContext3.reg_reads_global;
								regRead = regReads[registerIndex3];
								if regRead then
									regReadsList = regRead;
								else
									local newRegReadList = {};
									regReads[registerIndex3] = newRegReadList;
									regReadsList = newRegReadList;
								end;
								table.insert(regReadsList, regWriteInfo);
								local _ = regWriteInfo.code;
								regReadsList = registerContext3.neglected[registerIndex3];
								if regReadsList then
									for _ = #regReadsList, 1, -1 do

									end;
								end;
							end;
							for _, regIndex in ipairs(registerList) do
								if registerHasVariable[regIndex] and not registerHasVariableMap[regIndex] then
									local regHasVarRange = registerContext3.reg_has_var[regIndex];
									for registerIndex = regHasVarRange.beginning, regHasVarRange.ending do
										registerContext3.reg_has_var[registerIndex] = nil;
									end;
								end;
								registerContext3.uncertain_regs[regIndex] = nil;
								local regWriteInfo = {
									code = demand, 
									location = registerContext3.location, 
									reg_range = registerRangeSet
								};
								local regWrites = registerContext3.reg_writes;
								local regWrite = regWrites[regIndex];
								local regWritesList;
								if regWrite then
									regWritesList = regWrite;
								else
									local newRegWriteList = {};
									regWrites[regIndex] = newRegWriteList;
									regWritesList = newRegWriteList;
								end;
								table.insert(regWritesList, regWriteInfo);
								regWrites = registerContext3.reg_writes_global;
								regWrite = regWrites[regIndex];
								if regWrite then
									regWritesList = regWrite;
								else
									local registerWritesMap = {};
									regWrites[regIndex] = registerWritesMap;
									regWritesList = registerWritesMap;
								end;
								table.insert(regWritesList, regWriteInfo);
								regWritesList = {
									hl_index = registerContext3.location.hl_index, 
									start_code = regWriteInfo.code, 
									register = regIndex
								};
								if registerContext3.neglected[regIndex] then

								end;
								local neglectedRegistersMap = registerContext3.neglected;
								local cachedRegisterValue = neglectedRegistersMap[regIndex];
								if cachedRegisterValue then
									regWrite = cachedRegisterValue;
								else
									local newRegisterValue = {};
									neglectedRegistersMap[regIndex] = newRegisterValue;
									regWrite = newRegisterValue;
								end;
								table.insert(regWrite, regWritesList);
							end;
						end;
						if #registerList > 1 and demand.opname ~= "NAMECALL" then
							defineVariable(registerContext3, registerRangeSet);
						end;
						for _, registerIndex in ipairs(registerList) do
							if #forcedDefinitions > 0 then
								for _, registerData in ipairs(forcedDefinitions) do
									local registerRange = registerData.register_range;
									local registerBeginning = registerRange.beginning;
									local registerEnding = registerRange.ending;
									local isRegisterInRange = false;
									if registerBeginning <= registerIndex then
										isRegisterInRange = registerIndex <= registerEnding;
									end;
									if isRegisterInRange then
										defineVariable(registerContext3, registerData.register_range, demand.code_index, nil, true);
									end;
									addChild(registerContext3, registerData, demand.code_index);
								end;
								table.clear(forcedDefinitions);
							elseif not registerHasVariableMap[registerIndex] then

							end;
						end;
						if #forcedDefinitions > 0 then
							print(forcedDefinitions);
							error("Failed to handle forcedef");
						end;
					end;
				end;
				createRegisterSnapshot = function(dataSnapshot)
					local clonedRegReads = deepCloneTable(dataSnapshot.reg_reads);
					local regReadsGlobal = deepCloneTable(dataSnapshot.reg_reads_global);
					return {
						reg_has_var = table.clone(dataSnapshot.reg_has_var), 
						reg_forced_var = dataSnapshot.reg_forced_var, 
						reg_reads = clonedRegReads, 
						reg_reads_global = regReadsGlobal, 
						reg_writes = {}, 
						reg_writes_global = dataSnapshot.reg_writes_global, 
						demands = dataSnapshot.demands, 
						observations = dataSnapshot.observations, 
						uncertain_regs = dataSnapshot.uncertain_regs, 
						preprocess_task_index = dataSnapshot.preprocess_task_index, 
						neglected = {}, 
						location = dataSnapshot.location
					};
				end;
				isAncestor = function(currentState, previousState)
					for neglectedRegisterIndex, neglectedRegisterValue in pairs(previousState.neglected) do
						currentState.neglected[neglectedRegisterIndex] = neglectedRegisterValue;
					end;
					for writtenRegisterIndex, writeInfoList in pairs(previousState.reg_writes) do
						if #writeInfoList > 0 and not previousState.reg_has_var[writtenRegisterIndex] then
							currentState.uncertain_regs[writtenRegisterIndex] = {
								register_range = {
									beginning = writtenRegisterIndex, 
									ending = writtenRegisterIndex
								}, 
								index = writeInfoList[1].code.index, 
								location = previousState.location
							};
						end;
					end;
					for readRegisterIndex, readRegisterEntries in pairs(previousState.reg_reads) do
						local globalRegisterReadsMap = currentState.reg_reads_global;
						local globalRegisterReads = globalRegisterReadsMap[readRegisterIndex];
						local registerReads;
						if globalRegisterReads then
							registerReads = globalRegisterReads;
						else
							local newRegisterReads = {};
							globalRegisterReadsMap[readRegisterIndex] = newRegisterReads;
							registerReads = newRegisterReads;
						end;
						for _, element in ipairs(readRegisterEntries) do
							table.insert(registerReads, element);
						end;
					end;
				end;
				propagateVariable = nil;
				cycleCount = 0;
				childCount = 0;
				variableCount = #jumpTable;
				locationVariables = {};
				analyzeCondition = function(context, bytecodeInstruction, highlightData, condition, conditionCheck3)
					childCount = childCount + 1;
					progressUpdateFunction(childCount, variableCount, "<VarAnalysisCycle" .. cycleCount .. ">");
					local highLevelIndex = bytecodeInstruction.hl_index;
					local currentLocation = context.location;
					local newLocation = {
						hl_index = highLevelIndex, 
						depth = if condition then currentLocation.depth + 1 else currentLocation.depth, 
						parent = if condition then currentLocation else currentLocation.parent, 
						last_loop = currentLocation.last_loop
					};
					if conditionCheck3 then
						newLocation.last_loop = newLocation;
					end;
					context.location = newLocation;
					if highlightData then
						instructionList[bytecodeInstruction.hl_index] = highlightData.hl_index;
					end;
					if not visitorDestinationMap[bytecodeInstruction.hl_index] then
						visitorDestinationMap[bytecodeInstruction.hl_index] = context.location;
					end;
					if bytecodeInstruction.type == "sequential" then
						for _, dataItem in ipairs(bytecodeInstruction.data) do
							instructionDataMap[dataItem] = instructionDataMap[bytecodeInstruction.hl_index];
							analyzeCondition(context, jumpTable[dataItem], highlightData);
						end;
					elseif bytecodeInstruction.type == "single" then
						processDemands(context, bytecodeInstruction.hl_index, bytecodeInstruction.data, bytecodeInstruction._visitor);
					elseif not (bytecodeInstruction.type ~= "ifthen") or bytecodeInstruction.type == "ifthenelse" then
						local instructionData = bytecodeInstruction.data;
						context.location = propagateVariable(context, instructionData.condition, bytecodeInstruction);
						highLevelIndex = createRegisterSnapshot(context);
						currentLocation = nil;
						if instructionData.else_ then
							currentLocation = createRegisterSnapshot(context);
							instructionDataMap[instructionData.else_] = bytecodeInstruction.hl_index;
							currentLocation.neglected = {};
							analyzeCondition(currentLocation, jumpTable[instructionData.else_], bytecodeInstruction, true);
						end;
						instructionDataMap[instructionData.pass] = bytecodeInstruction.hl_index;
						highLevelIndex.neglected = {};
						analyzeCondition(highLevelIndex, jumpTable[instructionData.pass], bytecodeInstruction, true);
						isAncestor(context, highLevelIndex);
						if instructionData.else_ then
							isAncestor(context, currentLocation);
						end;
					elseif bytecodeInstruction.type == "oneblockwhile" then
						local newContext = createRegisterSnapshot(context);
						newContext.neglected = {};
						highLevelIndex = bytecodeInstruction.data;
						instructionDataMap[highLevelIndex.code] = bytecodeInstruction.hl_index;
						currentLocation = highLevelIndex.for_info;
						if currentLocation and not currentLocation.variables then
							newLocation = nil;
							if currentLocation.type == "numeric" then
								local indexRegister = currentLocation.args.index_reg;
								local indexRange = {
									beginning = indexRegister, 
									ending = indexRegister
								};
								indexRegister = {};
								local localTable4 = {};
								local v1223_8 = allocateVariable;
								local variableName = "var" .. tostring(variableCounter);
								local variableCounter = variableIndexMap[variableName];
								local uniqueVariableName = variableName;
								local counter = variableCounter or 1;
								while variableMap[uniqueVariableName] or globalNameCache[uniqueVariableName] do
									counter = counter + 1;
									uniqueVariableName = variableName .. "_" .. counter;
								end;
								variableIndexMap[variableName] = counter;
								local uniqueVariableNameCopy = uniqueVariableName;
								variableCounter = variableCounter + 1;
								local result = v1223_8(uniqueVariableNameCopy, indexRange, localTable4);
								arrayCopyFunction(indexRegister, 1, result);
								newLocation = indexRegister;
							elseif currentLocation.type == "generic" then
								local _ = currentLocation.args;
								local localTable5 = {};
								local variableCount = currentLocation.variable_count;
								newLocation = table.create(variableCount);
								for _ = 1, variableCount do
									local locationVariablesRegRange = currentLocation.variables_reg_range;
									local v1223_9 = allocateVariable;
									local variableName = "var" .. tostring(variableCounter);
									local variableCounter = variableIndexMap[variableName];
									local uniqueVariableName = variableName;
									local counter = variableCounter or 1;
									while variableMap[uniqueVariableName] or globalNameCache[uniqueVariableName] do
										counter = counter + 1;
										uniqueVariableName = variableName .. "_" .. counter;
									end;
									variableIndexMap[variableName] = counter;
									local uniqueVariableNameCopy = uniqueVariableName;
									variableCounter = variableCounter + 1;
									table.insert(newLocation, (v1223_9(uniqueVariableNameCopy, locationVariablesRegRange, localTable5)));
								end;
							else
								error((("Unknown for_info type \"%*\""):format(currentLocation.type)));
							end;
							currentLocation.variables = newLocation;
							locationVariables[currentLocation] = true;
							defineVariable(newContext.demands, currentLocation.variables_reg_range, bytecodeInstruction.index, context.location, nil, true);
						end;
						analyzeCondition(newContext, jumpTable[highLevelIndex.code], bytecodeInstruction, true, true);
						isAncestor(context, newContext);
					else
						error((("Unknown hl_block type %*"):format(bytecodeInstruction.type)));
					end;
					instructionCloneList[bytecodeInstruction.hl_index] = context;
				end;
				createCondition = function(context, condition, location, isTrue)
					local locationData = nil;
					if condition.type == "reg" then
						assert(condition.code);
						local conditionData = jumpTable[condition.code];
						local hlIndex = conditionData.hl_index;
						local locationInfo2 = context.location;
						locationData = {
							hl_index = hlIndex, 
							depth = if isTrue then locationInfo2.depth + 1 else locationInfo2.depth, 
							parent = if isTrue then locationInfo2 else locationInfo2.parent, 
							last_loop = locationInfo2.last_loop
						};
						context.location = locationData;
						analyzeCondition(context, conditionData, location, nil);
						return locationData;
					else
						local hlIndex = context.location.hl_index;
						local location = context.location;
						locationData = {
							hl_index = hlIndex, 
							depth = if isTrue then location.depth + 1 else location.depth, 
							parent = if isTrue then location else location.parent, 
							last_loop = location.last_loop
						};
						context.location = locationData;
						hlIndex = condition.lhs;
						if type(hlIndex) == "table" then
							if hlIndex.is_full_condition_t then
								createCondition(context, hlIndex, location, condition.type == "and");
							end;
							location = condition.rhs;
							if type(location) == "table" and location.is_full_condition_t then
								if condition.type == "and" then
									local newContext = createRegisterSnapshot(context);
									locationData = createCondition(newContext, location, location, nil);
									isAncestor(context, newContext);
									return locationData;
								else
									createCondition(context, location, location);
								end;
							end;
						end;
						return locationData;
					end;
				end;
				propagateVariable = createCondition;
				createVisitor = controlFlowEntries();
				processVisitors = false;
				foundFlag = {};
				processNode = {};
				while true do
					neglectedInstructions = {};
					cycleCount = cycleCount + 1;
					childCount = 0;
					if cycleCount > 1 then
						tempTable = {};
					end;
					instructionCloneList = {};
					instructionDataMap = {};
					instructionList = {};
					visitorDestinationMap = {};
					instructionMap = {};
					instructionCount = 0;
					removeRedundantJumps = {};
					processNode = {};
					for loopVariable in pairs(locationVariables) do
						loopVariable.variables = nil;
					end;
					locationVariables = {};
					local locationData = {
						hl_index = createVisitor.hl_index, 
						depth = 0, 
						parent = nil
					};
					analyzeCondition({
						reg_src = {}, 
						reg_all_src = {}, 
						reg_has_var = {}, 
						reg_forced_var = {}, 
						reg_reads = {}, 
						reg_reads_global = {}, 
						reg_writes = {}, 
						reg_writes_global = {}, 
						demands = foundFlag, 
						observations = processNode, 
						uncertain_regs = {}, 
						preprocess_task_index = instructionCount, 
						neglected = neglectedInstructions, 
						location = locationData
					}, createVisitor, createVisitor, true);
					if not next(foundFlag) then
						for _, item in ipairs(processNode) do
							local _ = item.location.parent.hl_index;
							if not foundFlag[item.info.wanted_definition_point] then
								defineVariable(foundFlag, item.info.register_range, item.info.wanted_definition_point, item.info.definition_location, item.info.is_predefinition);
							end;
						end;
						if cycleCount >= 4 then
							processVisitors = true;
							break;
						elseif not (#processNode ~= 0) then
							break;
						end;
					else
						break;
					end;
				end;
				if processVisitors then
					addWarningLine(1, prefixWarning .. ": Variable analysis failed. Output will have some incorrect variable assignments");
				end;
				for key, value in pairs(tempTable) do
					assert(#value > 0);
					local tempTable = table.create(#value);
					local defVarList = {};
					local lockVarList = {};
					local itemsList = {};
					for index = #value, 1, -1 do
						local item = value[index];
						local elementType = item.type;
						if elementType == "defvar" then
							table.insert(defVarList, item);
						elseif elementType == "lockvar" then
							table.insert(lockVarList, item);
						else
							table.insert(itemsList, item);
						end;
					end;
					for _, defVar in ipairs(defVarList) do
						table.insert(tempTable, defVar);
					end;
					for _, lockVar in ipairs(lockVarList) do
						table.insert(tempTable, lockVar);
					end;
					for _, otherVar in ipairs(itemsList) do
						table.insert(tempTable, otherVar);
					end;
					tempTable[key] = tempTable;
				end;
				benchmark:end_benchmark("Stack Incantation");
				benchmark:start_benchmark("AST Generation");
				neglectedInstructions = function(index, expressionIndex)
					local evaluatedExpression;
					if variableMap[expressionIndex] then
						evaluatedExpression = setValueWithConversion(expressionIndex, variableMap[expressionIndex]);
					else
						local cachedExpression = stack[expressionIndex];
						if not cachedExpression then
							addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(expressionIndex) .. "]");
							cachedExpression = setValue(expressionIndex, (createDefaultMetadata(true)));
						end;
						evaluatedExpression = cachedExpression;
					end;
					stack[index] = evaluatedExpression;
					environment[evaluatedExpression] = index;
				end;
				local function fastCallHandler(_)
					definePredefinedVariables();
				end;
				local function v2580(_)
					success = true;
				end;
				local opcodeHandlers = nil;
				opcodeHandlers = {
					FASTCALL = fastCallHandler, 
					FASTCALL1 = fastCallHandler, 
					FASTCALL2 = fastCallHandler, 
					FASTCALL2K = fastCallHandler, 
					FASTCALL3 = fastCallHandler, 
					FORNPREP = fastCallHandler, 
					FORGPREP = fastCallHandler, 
					FORGPREP_NEXT = fastCallHandler, 
					FORGPREP_INEXT = fastCallHandler, 
					PREPVARARGS = fastCallHandler, 
					JUMP = v2580, 
					JUMPBACK = v2580, 
					JUMPX = v2580, 
					JUMPIF = v2580, 
					JUMPIFNOT = v2580, 
					JUMPIFEQ = v2580, 
					JUMPIFLE = v2580, 
					JUMPIFLT = v2580, 
					JUMPIFNOTEQ = v2580, 
					JUMPIFNOTLE = v2580, 
					JUMPIFNOTLT = v2580, 
					JUMPXEQKNIL = v2580, 
					JUMPXEQKB = v2580, 
					JUMPXEQKN = v2580, 
					JUMPXEQKS = v2580, 
					FORNLOOP = v2580, 
					FORGLOOP = v2580, 
					GETVARARGS = function(instructionData)
						local byte2 = bit32.band(bit32.rshift(instructionData, 8), 255);
						local varargCount = bit32.band(bit32.rshift(instructionData, 16), 255) - 1;
						if varargCount == 0 then
							addWarningComment(prefixWarning .. ": Malformed varargs");
							return;
						else
							if varargCount == -1 then
								endIndex = byte2;
							end;
							processVariable(byte2, createVarArgs(byte2));
							return;
						end;
					end, 
					MOVE = function(instructionData)
						local index1 = bit32.band(bit32.rshift(instructionData, 8), 255);
						local index2 = bit32.band(bit32.rshift(instructionData, 16), 255);
						local evaluatedExpression2;
						if variableMap[index2] then
							evaluatedExpression2 = setValueWithConversion(index2, variableMap[index2]);
						else
							local cachedExpression2 = stack[index2];
							if not cachedExpression2 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(index2) .. "]");
								cachedExpression2 = setValue(index2, (createDefaultMetadata(true)));
							end;
							evaluatedExpression2 = cachedExpression2;
						end;
						stack[index1] = evaluatedExpression2;
						environment[evaluatedExpression2] = index1;
						processVariable(index1, getValue(index2));
					end, 
					LOADK = function(instruction)
						local index3 = bit32.band(bit32.rshift(instruction, 8), 255);
						local constantValue = constantsList[bit32.band(bit32.rshift(instruction, 16), 65535)];
						processVariable(index3, createConstant(index3, constantValue));
					end, 
					LOADKX = function(v2593)
						local index4 = bit32.band(bit32.rshift(v2593, 8), 255);
						local constantValue = codeList[instructionIndex + 1];
						if not constantValue then
							error("Expected aux");
						end;
						constantValue = constantsList[constantValue];
						processVariable(index4, createConstant(index4, constantValue));
					end, 
					LOADN = function(dataValue)
						local index5 = bit32.band(bit32.rshift(dataValue, 8), 255);
						buffer.writeu16(buffer8, 0, (bit32.band(bit32.rshift(dataValue, 16), 65535)));
						processVariable(index5, createConstant(index5, {
							type = 2, 
							value = buffer.readi16(buffer8, 0)
						}));
					end, 
					LOADNIL = function(v2598)
						local index6 = bit32.band(bit32.rshift(v2598, 8), 255);
						processVariable(index6, createValue(index6));
					end, 
					LOADB = function(unknownValue1)
						local index7 = bit32.band(bit32.rshift(unknownValue1, 8), 255);
						local v2602 = bit32.band(bit32.rshift(unknownValue1, 16), 255);
						assert(v2602 <= 1);
						processVariable(index7, createBoolean(index7, v2602 == 1));
					end, 
					NEWTABLE = function(v2603)
						local index8 = bit32.band(bit32.rshift(v2603, 8), 255);
						processVariable(index8, createNewTable(index8));
					end, 
					DUPTABLE = function(v2605)
						local index9 = bit32.band(bit32.rshift(v2605, 8), 255);
						processVariable(index9, createNewTable(index9));
					end, 
					SETTABLE = function(instruction2)
						local index10 = bit32.band(bit32.rshift(instruction2, 8), 255);
						local unknownValue2 = bit32.band(bit32.rshift(instruction2, 16), 255);
						local index11 = bit32.band(bit32.rshift(instruction2, 24), 255);
						local callbackFunction = evaluateExpressionFunction;
						local arg1 = unknownValue2;
						local evaluatedExpression3;
						if variableMap[index11] then
							evaluatedExpression3 = setValueWithConversion(index11, variableMap[index11]);
						else
							local cachedExpression3 = stack[index11];
							if not cachedExpression3 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(index11) .. "]");
								cachedExpression3 = setValue(index11, (createDefaultMetadata(true)));
							end;
							evaluatedExpression3 = cachedExpression3;
						end;
						if expressionReuseCache[evaluatedExpression3] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[evaluatedExpression3] = true;
						end;
						local arg2 = evaluatedExpression3;
						local evaluatedExpression4;
						if variableMap[index10] then
							evaluatedExpression4 = setValueWithConversion(index10, variableMap[index10]);
						else
							local cachedExpression4 = stack[index10];
							if not cachedExpression4 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(index10) .. "]");
								cachedExpression4 = setValue(index10, (createDefaultMetadata(true)));
							end;
							evaluatedExpression4 = cachedExpression4;
						end;
						if expressionReuseCache[evaluatedExpression4] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[evaluatedExpression4] = true;
						end;
						callbackFunction(arg1, arg2, evaluatedExpression4);
						processUnknown();
						definePredefinedVariables();
					end, 
					SETTABLEKS = function(unknownValue3)
						local byteValue2 = bit32.band(bit32.rshift(unknownValue3, 8), 255);
						local unknownValue4 = bit32.band(bit32.rshift(unknownValue3, 16), 255);
						local localConstants = constantsList;
						local constantIndex = codeList[instructionIndex + 1];
						if not constantIndex then
							error("Expected aux");
						end;
						local constantValue = localConstants[constantIndex];
						localConstants = evaluateExpressionFunction;
						local localV2620 = unknownValue4;
						constantIndex = createConstant(nil, constantValue);
						local evaluatedExpression1;
						if variableMap[byteValue2] then
							evaluatedExpression1 = setValueWithConversion(byteValue2, variableMap[byteValue2]);
						else
							local cachedExpression1 = stack[byteValue2];
							if not cachedExpression1 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(byteValue2) .. "]");
								cachedExpression1 = setValue(byteValue2, (createDefaultMetadata(true)));
							end;
							evaluatedExpression1 = cachedExpression1;
						end;
						if expressionReuseCache[evaluatedExpression1] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[evaluatedExpression1] = true;
						end;
						localConstants(localV2620, constantIndex, evaluatedExpression1);
						processUnknown();
						definePredefinedVariables();
					end, 
					SETTABLEN = function(inputInteger1)
						local byteValue3 = bit32.band(bit32.rshift(inputInteger1, 8), 255);
						local unknownValue5 = bit32.band(bit32.rshift(inputInteger1, 16), 255);
						local byteValue4 = bit32.band(bit32.rshift(inputInteger1, 24), 255) + 1;
						local localV1446 = evaluateExpressionFunction;
						local localV2629 = unknownValue5;
						local constantTable = createConstant(nil, {
							type = 2, 
							value = byteValue4
						});
						local evaluatedExpression2;
						if variableMap[byteValue3] then
							evaluatedExpression2 = setValueWithConversion(byteValue3, variableMap[byteValue3]);
						else
							local cachedExpression2 = stack[byteValue3];
							if not cachedExpression2 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(byteValue3) .. "]");
								cachedExpression2 = setValue(byteValue3, (createDefaultMetadata(true)));
							end;
							evaluatedExpression2 = cachedExpression2;
						end;
						if expressionReuseCache[evaluatedExpression2] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[evaluatedExpression2] = true;
						end;
						localV1446(localV2629, constantTable, evaluatedExpression2);
						processUnknown();
						definePredefinedVariables();
					end, 
					GETTABLE = function(inputInteger2)
						local byteValue5 = bit32.band(bit32.rshift(inputInteger2, 8), 255);
						local byteValue6 = bit32.band(bit32.rshift(inputInteger2, 16), 255);
						local evaluatedExpression3;
						if variableMap[byteValue6] then
							evaluatedExpression3 = setValueWithConversion(byteValue6, variableMap[byteValue6]);
						else
							local cachedExpression3 = stack[byteValue6];
							if not cachedExpression3 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(byteValue6) .. "]");
								cachedExpression3 = setValue(byteValue6, (createDefaultMetadata(true)));
							end;
							evaluatedExpression3 = cachedExpression3;
						end;
						if expressionReuseCache[evaluatedExpression3] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[evaluatedExpression3] = true;
						end;
						local l_v2639_0 = evaluatedExpression3;
						local byteValue7 = bit32.band(bit32.rshift(inputInteger2, 24), 255);
						if variableMap[byteValue7] then
							byteValue6 = setValueWithConversion(byteValue7, variableMap[byteValue7]);
						else
							local cachedExpression4 = stack[byteValue7];
							if not cachedExpression4 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(byteValue7) .. "]");
								cachedExpression4 = setValue(byteValue7, (createDefaultMetadata(true)));
							end;
							byteValue6 = cachedExpression4;
						end;
						if expressionReuseCache[byteValue6] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[byteValue6] = true;
						end;
						processVariable(byteValue5, createGetTableExpression(byteValue5, l_v2639_0, byteValue6));
					end, 
					GETTABLEKS = function(unknownValue6)
						local byteValue8 = bit32.band(bit32.rshift(unknownValue6, 8), 255);
						local constantIndex2 = bit32.band(bit32.rshift(unknownValue6, 16), 255);
						local evaluatedExpression4;
						if variableMap[constantIndex2] then
							evaluatedExpression4 = setValueWithConversion(constantIndex2, variableMap[constantIndex2]);
						else
							local cachedExpression5 = stack[constantIndex2];
							if not cachedExpression5 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(constantIndex2) .. "]");
								cachedExpression5 = setValue(constantIndex2, (createDefaultMetadata(true)));
							end;
							evaluatedExpression4 = cachedExpression5;
						end;
						if expressionReuseCache[evaluatedExpression4] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[evaluatedExpression4] = true;
						end;
						local l_v2647_0 = evaluatedExpression4;
						constantIndex2 = constantsList;
						local auxiliaryValue = codeList[instructionIndex + 1];
						if not auxiliaryValue then
							error("Expected aux");
						end;
						evaluatedExpression4 = constantIndex2[auxiliaryValue];
						processVariable(byteValue8, createGetTableExpression(byteValue8, l_v2647_0, createConstant(nil, evaluatedExpression4)));
					end, 
					GETTABLEN = function(inputInteger3)
						local byteValue9 = bit32.band(bit32.rshift(inputInteger3, 8), 255);
						local byteValue10 = bit32.band(bit32.rshift(inputInteger3, 16), 255);
						local evaluatedExpression5;
						if variableMap[byteValue10] then
							evaluatedExpression5 = setValueWithConversion(byteValue10, variableMap[byteValue10]);
						else
							local evaluatedExpression = stack[byteValue10];
							if not evaluatedExpression then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(byteValue10) .. "]");
								evaluatedExpression = setValue(byteValue10, (createDefaultMetadata(true)));
							end;
							evaluatedExpression5 = evaluatedExpression;
						end;
						if expressionReuseCache[evaluatedExpression5] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[evaluatedExpression5] = true;
						end;
						local l_v2654_0 = evaluatedExpression5;
						evaluatedExpression5 = bit32.band(bit32.rshift(inputInteger3, 24), 255) + 1;
						processVariable(byteValue9, createGetTableExpression(byteValue9, l_v2654_0, createConstant(nil, {
							type = 2, 
							value = evaluatedExpression5
						})));
					end, 
					SETLIST = function(sourceInteger)
						local byteValue8 = bit32.band(bit32.rshift(sourceInteger, 8), 255);
						local unknownValue7 = bit32.band(bit32.rshift(sourceInteger, 16), 255);
						local byteValue24 = bit32.band(bit32.rshift(sourceInteger, 24), 255) - 1;
						local auxiliaryValue = codeList[instructionIndex + 1];
						if not auxiliaryValue then
							error("Expected aux");
						end;
						local auxiliaryValueIncremented = auxiliaryValue;
						if variableMap[byteValue8] then
							auxiliaryValue = setValueWithConversion(byteValue8, variableMap[byteValue8]);
						else
							local evaluatedExpression2 = stack[byteValue8];
							if not evaluatedExpression2 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(byteValue8) .. "]");
								evaluatedExpression2 = setValue(byteValue8, (createDefaultMetadata(true)));
							end;
							auxiliaryValue = evaluatedExpression2;
						end;
						local initialByteValue16 = unknownValue7;
						local loopEndValue = if byteValue24 == -1 then endIndex else unknownValue7 + byteValue24 - 1;
						for loopIndex = initialByteValue16, loopEndValue do
							local v1446Copy = evaluateExpressionFunction;
							local byteValue8Copy = byteValue8;
							local valueTable = createConstant(nil, {
								type = 2, 
								value = auxiliaryValueIncremented
							});
							local evaluatedValue;
							if variableMap[loopIndex] then
								evaluatedValue = setValueWithConversion(loopIndex, variableMap[loopIndex]);
							else
								local evaluatedExpression3 = stack[loopIndex];
								if not evaluatedExpression3 then
									addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(loopIndex) .. "]");
									evaluatedExpression3 = setValue(loopIndex, (createDefaultMetadata(true)));
								end;
								evaluatedValue = evaluatedExpression3;
							end;
							if expressionReuseCache[evaluatedValue] then
								addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
							else
								expressionReuseCache[evaluatedValue] = true;
							end;
							v1446Copy(byteValue8Copy, valueTable, evaluatedValue);
							processUnknown();
							auxiliaryValueIncremented = auxiliaryValueIncremented + 1;
						end;
						setValue(byteValue8, auxiliaryValue);
						definePredefinedVariables();
					end, 
					GETUPVAL = function(unknownValue8)
						local byteValue8_2 = bit32.band(bit32.rshift(unknownValue8, 8), 255);
						local byteValue16_2 = bit32.band(bit32.rshift(unknownValue8, 16), 255);
						assert(parentFunction);
						local upvalueValue = parentFunction.upvalues[byteValue16_2];
						processVariable(byteValue8_2, setValueWithConversion(byteValue8_2, upvalueValue.name));
					end, 
					SETUPVAL = function(unknownValue9)
						local byteValue8_3 = bit32.band(bit32.rshift(unknownValue9, 8), 255);
						local byteValue16_3 = bit32.band(bit32.rshift(unknownValue9, 16), 255);
						assert(parentFunction);
						local upvalue = parentFunction.upvalues[byteValue16_3];
						local v1435Copy = setVariable;
						local upvalueName = upvalue.name;
						local evaluatedValue2;
						if variableMap[byteValue8_3] then
							evaluatedValue2 = setValueWithConversion(byteValue8_3, variableMap[byteValue8_3]);
						else
							local evaluatedExpression4 = stack[byteValue8_3];
							if not evaluatedExpression4 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(byteValue8_3) .. "]");
								evaluatedExpression4 = setValue(byteValue8_3, (createDefaultMetadata(true)));
							end;
							evaluatedValue2 = evaluatedExpression4;
						end;
						if expressionReuseCache[evaluatedValue2] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[evaluatedValue2] = true;
						end;
						v1435Copy(upvalueName, evaluatedValue2);
						processUnknown();
						definePredefinedVariables();
					end, 
					GETIMPORT = function(v2684)
						local byteValue8_4 = bit32.band(bit32.rshift(v2684, 8), 255);
						local auxiliaryValue2 = codeList[instructionIndex + 1];
						if not auxiliaryValue2 then
							error("Expected aux");
						end;
						auxiliaryValue2 = processConstant(byteValue8_4, auxiliaryValue2);
						assert(auxiliaryValue2);
						processVariable(byteValue8_4, auxiliaryValue2);
					end, 
					GETGLOBAL = function(v2687)
						local byteValue8_5 = bit32.band(bit32.rshift(v2687, 8), 255);
						local localConstants1_1 = constantsList;
						local constantIndex = codeList[instructionIndex + 1];
						if not constantIndex then
							error("Expected aux");
						end;
						local constantValue = localConstants1_1[constantIndex];
						processVariable(byteValue8_5, createGlobal(byteValue8_5, constantValue));
					end, 
					SETGLOBAL = function(v2692)
						local byteValue8_6 = bit32.band(bit32.rshift(v2692, 8), 255);
						local localConstants1_2 = constantsList;
						local constantIndex2 = codeList[instructionIndex + 1];
						if not constantIndex2 then
							error("Expected aux");
						end;
						local constantValue = localConstants1_2[constantIndex2];
						localConstants1_2 = setGlobalFunction;
						local localConstantValue = constantValue;
						local expressionResult;
						if variableMap[byteValue8_6] then
							expressionResult = setValueWithConversion(byteValue8_6, variableMap[byteValue8_6]);
						else
							local evaluatedExpression = stack[byteValue8_6];
							if not evaluatedExpression then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(byteValue8_6) .. "]");
								evaluatedExpression = setValue(byteValue8_6, (createDefaultMetadata(true)));
							end;
							expressionResult = evaluatedExpression;
						end;
						if expressionReuseCache[expressionResult] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[expressionResult] = true;
						end;
						localConstants1_2(localConstantValue, expressionResult);
						processUnknown();
						definePredefinedVariables();
					end, 
					NAMECALL = function(unknownValue10)
						local byte1 = bit32.band(bit32.rshift(unknownValue10, 8), 255);
						local byte2 = bit32.band(bit32.rshift(unknownValue10, 16), 255);
						local constantsTable = constantsList;
						local auxiliaryIndex = codeList[instructionIndex + 1];
						if not auxiliaryIndex then
							error("Expected aux");
						end;
						constantValue = constantsTable[auxiliaryIndex] or {
							type = 3, 
							value = "<UNK>"
						};
						assert(constantValue);
						local expressionValue;
						if variableMap[byte2] then
							expressionValue = setValueWithConversion(byte2, variableMap[byte2]);
						else
							local cachedExpressionValue = stack[byte2];
							if not cachedExpressionValue then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(byte2) .. "]");
								cachedExpressionValue = setValue(byte2, (createDefaultMetadata(true)));
							end;
							expressionValue = cachedExpressionValue;
						end;
						stack[byte1] = expressionValue;
						environment[expressionValue] = byte1;
						if variableMap[byte2] then
							expressionValue = setValueWithConversion(byte2, variableMap[byte2]);
						else
							local expressionValueNilFallback = stack[byte2];
							if not expressionValueNilFallback then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(byte2) .. "]");
								expressionValueNilFallback = setValue(byte2, (createDefaultMetadata(true)));
							end;
							expressionValue = expressionValueNilFallback;
						end;
						constantsTable = createConstantIndex(byte1, expressionValue, constantValue, true);
						assert(constantsTable);
						processVariable(byte1, constantsTable);
					end, 
					RETURN = function(unknownValue11)
						local byte1Shifted = bit32.band(bit32.rshift(unknownValue11, 8), 255);
						local byte2Shifted = bit32.band(bit32.rshift(unknownValue11, 16), 255) - 1;
						if byte2Shifted == 0 and instructionNumber == #instructionList then
							linesHadSkippedReturnMap[linesList] = true;
							definePredefinedVariables();
							return;
						else
							evaluateExpressionsInRangeFunction(byte1Shifted, byte2Shifted);
							processUnknown();
							definePredefinedVariables();
							return;
						end;
					end, 
					CALL = function(combinedBytes)
						local byte1Extracted = bit32.band(bit32.rshift(combinedBytes, 8), 255);
						local byte2Calculated = bit32.band(bit32.rshift(combinedBytes, 16), 255) - 1;
						local byte3Extracted = bit32.band(bit32.rshift(combinedBytes, 24), 255) - 1;
						if byte2Calculated == -1 then
							byte2Calculated = endIndex - byte1Extracted;
						end;
						local expressionResultCombined = evaluateExpression(byte1Extracted, byte2Calculated, byte3Extracted, constantValue and constantValue.value);
						constantValue = nil;
						if byte3Extracted == -1 then
							endIndex = byte1Extracted;
						end;
						if byte3Extracted ~= 0 then
							processVariable(byte1Extracted, expressionResultCombined);
							return;
						else
							if expressionResultCombined.t == "name" then
								print(debug.traceback());
							else
								local callInfo = {
									t = "call", 
									lines = linesList, 
									reads = {}, 
									writes = {}, 
									func = expressionResultCombined.func, 
									args = expressionResultCombined.args
								};
								addDependency(callInfo, expressionResultCombined.func);
								for _, argument in ipairs(expressionResultCombined.args) do
									addDependency(callInfo, argument);
								end;
								lineData = callInfo;
								processUnknown();
							end;
							definePredefinedVariables();
							return;
						end;
					end, 
					CLOSEUPVALS = function(v2719)
						local byteValue = bit32.band(bit32.rshift(v2719, 8), 255);
						for tableIndex in pairs(table.clone(visitedExpressionsMap)) do
							if byteValue <= tableIndex then
								booleanMap[tableIndex] = nil;
								visitedExpressionsMap[tableIndex] = nil;
							end;
						end;
						definePredefinedVariables();
					end, 
					DUPCLOSURE = function(combinedData)
						local byteValueExtracted = bit32.band(bit32.rshift(combinedData, 8), 255);
						local constantEntry = constantsList[bit32.band(bit32.rshift(combinedData, 16), 65535)];
						if constantEntry.type ~= 6 then
							globalFailedInstructionsCount = globalFailedInstructionsCount + 1;
							error("Invalid DUPCLOSURE");
						end;
						local closureValue = bytecodeConstants[constantEntry.value];
						createFunctionInfo(byteValueExtracted, closureValue);
					end, 
					NEWCLOSURE = function(combinedData2)
						local byteValueExtracted2 = bit32.band(bit32.rshift(combinedData2, 8), 255);
						local protoValue = protosList[bit32.band(bit32.rshift(combinedData2, 16), 65535)];
						createFunctionInfo(byteValueExtracted2, protoValue);
					end, 
					ADD = function(combinedData3)
						local byteValueExtracted3 = bit32.band(bit32.rshift(combinedData3, 8), 255);
						local byteValueExtracted4 = bit32.band(bit32.rshift(combinedData3, 16), 255);
						local byteValueExtracted5 = bit32.band(bit32.rshift(combinedData3, 24), 255);
						local localV1516 = processVariable;
						local expressionValue1 = byteValueExtracted3;
						local expressionFunction1 = createAdditionExpression;
						local l_v2730_1 = byteValueExtracted3;
						local expressionValue2;
						if variableMap[byteValueExtracted4] then
							expressionValue2 = setValueWithConversion(byteValueExtracted4, variableMap[byteValueExtracted4]);
						else
							local expressionValue3 = stack[byteValueExtracted4];
							if not expressionValue3 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(byteValueExtracted4) .. "]");
								expressionValue3 = setValue(byteValueExtracted4, (createDefaultMetadata(true)));
							end;
							expressionValue2 = expressionValue3;
						end;
						if expressionReuseCache[expressionValue2] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[expressionValue2] = true;
						end;
						local l_v2737_0 = expressionValue2;
						local expressionValue4;
						if variableMap[byteValueExtracted5] then
							expressionValue4 = setValueWithConversion(byteValueExtracted5, variableMap[byteValueExtracted5]);
						else
							local expressionValue5 = stack[byteValueExtracted5];
							if not expressionValue5 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(byteValueExtracted5) .. "]");
								expressionValue5 = setValue(byteValueExtracted5, (createDefaultMetadata(true)));
							end;
							expressionValue4 = expressionValue5;
						end;
						if expressionReuseCache[expressionValue4] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[expressionValue4] = true;
						end;
						localV1516(expressionValue1, expressionFunction1(l_v2730_1, l_v2737_0, expressionValue4));
					end, 
					ADDK = function(combinedBytes)
						local byte2 = bit32.band(bit32.rshift(combinedBytes, 8), 255);
						local byte3 = bit32.band(bit32.rshift(combinedBytes, 16), 255);
						local constantValue = constantsList[bit32.band(bit32.rshift(combinedBytes, 24), 255)];
						if constantValue.type ~= 2 then
							addWarningComment(prefixWarning .. ": Malformed ADDK (constant wasn't a number)");
						end;
						local functionCall2 = processVariable;
						local byte2Local1 = byte2;
						local expressionFunction2 = createAdditionExpression;
						local l_v2743_1 = byte2;
						local expressionValue6;
						if variableMap[byte3] then
							expressionValue6 = setValueWithConversion(byte3, variableMap[byte3]);
						else
							local expressionValue7 = stack[byte3];
							if not expressionValue7 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(byte3) .. "]");
								expressionValue7 = setValue(byte3, (createDefaultMetadata(true)));
							end;
							expressionValue6 = expressionValue7;
						end;
						if expressionReuseCache[expressionValue6] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[expressionValue6] = true;
						end;
						functionCall2(byte2Local1, expressionFunction2(l_v2743_1, expressionValue6, createConstant(nil, constantValue)));
					end, 
					SUB = function(combinedBytes2)
						local byte5 = bit32.band(bit32.rshift(combinedBytes2, 8), 255);
						local byte6 = bit32.band(bit32.rshift(combinedBytes2, 16), 255);
						local byte7 = bit32.band(bit32.rshift(combinedBytes2, 24), 255);
						local functionCall3 = processVariable;
						local byte5Local1 = byte5;
						local expressionFunction3 = createSubtractionExpression;
						local l_v2753_1 = byte5;
						local expressionValue8;
						if variableMap[byte6] then
							expressionValue8 = setValueWithConversion(byte6, variableMap[byte6]);
						else
							local expressionValue9 = stack[byte6];
							if not expressionValue9 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(byte6) .. "]");
								expressionValue9 = setValue(byte6, (createDefaultMetadata(true)));
							end;
							expressionValue8 = expressionValue9;
						end;
						if expressionReuseCache[expressionValue8] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[expressionValue8] = true;
						end;
						local l_v2760_0 = expressionValue8;
						local expressionValue10;
						if variableMap[byte7] then
							expressionValue10 = setValueWithConversion(byte7, variableMap[byte7]);
						else
							local expressionValue11 = stack[byte7];
							if not expressionValue11 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(byte7) .. "]");
								expressionValue11 = setValue(byte7, (createDefaultMetadata(true)));
							end;
							expressionValue10 = expressionValue11;
						end;
						if expressionReuseCache[expressionValue10] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[expressionValue10] = true;
						end;
						functionCall3(byte5Local1, expressionFunction3(l_v2753_1, l_v2760_0, expressionValue10));
					end, 
					SUBK = function(combinedBytes3)
						local byte8 = bit32.band(bit32.rshift(combinedBytes3, 8), 255);
						local byte9 = bit32.band(bit32.rshift(combinedBytes3, 16), 255);
						local constantValue2 = constantsList[bit32.band(bit32.rshift(combinedBytes3, 24), 255)];
						if constantValue2.type ~= 2 then
							addWarningComment(prefixWarning .. ": Malformed SUBK (constant wasn't a number)");
						end;
						local functionCall4 = processVariable;
						local byte8Local1 = byte8;
						local expressionFunction4 = createSubtractionExpression;
						local l_v2766_1 = byte8;
						local expressionValue12;
						if variableMap[byte9] then
							expressionValue12 = setValueWithConversion(byte9, variableMap[byte9]);
						else
							local expressionValue13 = stack[byte9];
							if not expressionValue13 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(byte9) .. "]");
								expressionValue13 = setValue(byte9, (createDefaultMetadata(true)));
							end;
							expressionValue12 = expressionValue13;
						end;
						if expressionReuseCache[expressionValue12] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[expressionValue12] = true;
						end;
						functionCall4(byte8Local1, expressionFunction4(l_v2766_1, expressionValue12, createConstant(nil, constantValue2)));
					end, 
					MUL = function(colorValue)
						local byte9 = bit32.band(bit32.rshift(colorValue, 8), 255);
						local redChannel = bit32.band(bit32.rshift(colorValue, 16), 255);
						local blueChannel = bit32.band(bit32.rshift(colorValue, 24), 255);
						local v1516_local = processVariable;
						local v2776_local = byte9;
						local v1613_local = createMultiplicationExpression;
						local l_v2776_1 = byte9;
						local expressionResult1;
						if variableMap[redChannel] then
							expressionResult1 = setValueWithConversion(redChannel, variableMap[redChannel]);
						else
							local cachedExpression1 = stack[redChannel];
							if not cachedExpression1 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(redChannel) .. "]");
								cachedExpression1 = setValue(redChannel, (createDefaultMetadata(true)));
							end;
							expressionResult1 = cachedExpression1;
						end;
						if expressionReuseCache[expressionResult1] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[expressionResult1] = true;
						end;
						local l_v2783_0 = expressionResult1;
						local expressionResult2;
						if variableMap[blueChannel] then
							expressionResult2 = setValueWithConversion(blueChannel, variableMap[blueChannel]);
						else
							local cachedExpression2 = stack[blueChannel];
							if not cachedExpression2 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(blueChannel) .. "]");
								cachedExpression2 = setValue(blueChannel, (createDefaultMetadata(true)));
							end;
							expressionResult2 = cachedExpression2;
						end;
						if expressionReuseCache[expressionResult2] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[expressionResult2] = true;
						end;
						v1516_local(v2776_local, v1613_local(l_v2776_1, l_v2783_0, expressionResult2));
					end, 
					MULK = function(encodedValue1)
						local byteValue1 = bit32.band(bit32.rshift(encodedValue1, 8), 255);
						local greenChannel = bit32.band(bit32.rshift(encodedValue1, 16), 255);
						local constantValue = constantsList[bit32.band(bit32.rshift(encodedValue1, 24), 255)];
						if constantValue.type ~= 2 then
							addWarningComment(prefixWarning .. ": Malformed MULK (constant wasn't a number)");
						end;
						local v1516_local2 = processVariable;
						local v2789_local1 = byteValue1;
						local v1613_local2 = createMultiplicationExpression;
						local l_v2789_1 = byteValue1;
						local expressionResult3;
						if variableMap[greenChannel] then
							expressionResult3 = setValueWithConversion(greenChannel, variableMap[greenChannel]);
						else
							local cachedExpression3 = stack[greenChannel];
							if not cachedExpression3 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(greenChannel) .. "]");
								cachedExpression3 = setValue(greenChannel, (createDefaultMetadata(true)));
							end;
							expressionResult3 = cachedExpression3;
						end;
						if expressionReuseCache[expressionResult3] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[expressionResult3] = true;
						end;
						v1516_local2(v2789_local1, v1613_local2(l_v2789_1, expressionResult3, createConstant(nil, constantValue)));
					end, 
					DIV = function(encodedValue2)
						local byteValue2 = bit32.band(bit32.rshift(encodedValue2, 8), 255);
						local alphaChannel = bit32.band(bit32.rshift(encodedValue2, 16), 255);
						local channel4 = bit32.band(bit32.rshift(encodedValue2, 24), 255);
						local v1516_local3 = processVariable;
						local v2799_local1 = byteValue2;
						local v1624_local = createDivisionExpression;
						local l_v2799_1 = byteValue2;
						local expressionResult4;
						if variableMap[alphaChannel] then
							expressionResult4 = setValueWithConversion(alphaChannel, variableMap[alphaChannel]);
						else
							local cachedExpression4 = stack[alphaChannel];
							if not cachedExpression4 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(alphaChannel) .. "]");
								cachedExpression4 = setValue(alphaChannel, (createDefaultMetadata(true)));
							end;
							expressionResult4 = cachedExpression4;
						end;
						if expressionReuseCache[expressionResult4] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[expressionResult4] = true;
						end;
						local l_v2806_0 = expressionResult4;
						local expressionResult5;
						if variableMap[channel4] then
							expressionResult5 = setValueWithConversion(channel4, variableMap[channel4]);
						else
							local cachedExpression5 = stack[channel4];
							if not cachedExpression5 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(channel4) .. "]");
								cachedExpression5 = setValue(channel4, (createDefaultMetadata(true)));
							end;
							expressionResult5 = cachedExpression5;
						end;
						if expressionReuseCache[expressionResult5] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[expressionResult5] = true;
						end;
						v1516_local3(v2799_local1, v1624_local(l_v2799_1, l_v2806_0, expressionResult5));
					end, 
					DIVK = function(encodedValue3)
						local byteValue3 = bit32.band(bit32.rshift(encodedValue3, 8), 255);
						local bandedValue16 = bit32.band(bit32.rshift(encodedValue3, 16), 255);
						local constantValue24 = constantsList[bit32.band(bit32.rshift(encodedValue3, 24), 255)];
						if constantValue24.type ~= 2 then
							addWarningComment(prefixWarning .. ": Malformed DIVK (constant wasn't a number)");
						end;
						local v1516_7 = processVariable;
						local v2812_0 = byteValue3;
						local v1624_1 = createDivisionExpression;
						local l_v2812_1 = byteValue3;
						local evaluatedExpression16;
						if variableMap[bandedValue16] then
							evaluatedExpression16 = setValueWithConversion(bandedValue16, variableMap[bandedValue16]);
						else
							local cachedExpression16 = stack[bandedValue16];
							if not cachedExpression16 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(bandedValue16) .. "]");
								cachedExpression16 = setValue(bandedValue16, (createDefaultMetadata(true)));
							end;
							evaluatedExpression16 = cachedExpression16;
						end;
						if expressionReuseCache[evaluatedExpression16] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[evaluatedExpression16] = true;
						end;
						v1516_7(v2812_0, v1624_1(l_v2812_1, evaluatedExpression16, createConstant(nil, constantValue24)));
					end, 
					SUBRK = function(inputInteger)
						local bandedValue8 = bit32.band(bit32.rshift(inputInteger, 8), 255);
						local constantValue16 = constantsList[bit32.band(bit32.rshift(inputInteger, 16), 255)];
						local bandedValue24 = bit32.band(bit32.rshift(inputInteger, 24), 255);
						if constantValue16.type ~= 2 then
							addWarningComment(prefixWarning .. ": Malformed SUBRK (constant wasn't a number)");
						end;
						local v1516_8 = processVariable;
						local v2822_0 = bandedValue8;
						local v1602_2 = createSubtractionExpression;
						local l_v2822_1 = bandedValue8;
						local unknownObject1 = createConstant(nil, constantValue16);
						local evaluatedExpression24;
						if variableMap[bandedValue24] then
							evaluatedExpression24 = setValueWithConversion(bandedValue24, variableMap[bandedValue24]);
						else
							local cachedExpression24 = stack[bandedValue24];
							if not cachedExpression24 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(bandedValue24) .. "]");
								cachedExpression24 = setValue(bandedValue24, (createDefaultMetadata(true)));
							end;
							evaluatedExpression24 = cachedExpression24;
						end;
						if expressionReuseCache[evaluatedExpression24] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[evaluatedExpression24] = true;
						end;
						v1516_8(v2822_0, v1602_2(l_v2822_1, unknownObject1, evaluatedExpression24));
					end, 
					DIVRK = function(inputInteger2)
						local bandedValue8_2 = bit32.band(bit32.rshift(inputInteger2, 8), 255);
						local constantValue16_2 = constantsList[bit32.band(bit32.rshift(inputInteger2, 16), 255)];
						local bandedValue24_2 = bit32.band(bit32.rshift(inputInteger2, 24), 255);
						if constantValue16_2.type ~= 2 then
							addWarningComment(prefixWarning .. ": Malformed DIVRK (constant wasn't a number)");
						end;
						local v1516_9 = processVariable;
						local v2833_0 = bandedValue8_2;
						local v1624_2 = createDivisionExpression;
						local l_v2833_1 = bandedValue8_2;
						local unknownObject2 = createConstant(nil, constantValue16_2);
						local evaluatedExpression24_2;
						if variableMap[bandedValue24_2] then
							evaluatedExpression24_2 = setValueWithConversion(bandedValue24_2, variableMap[bandedValue24_2]);
						else
							local cachedExpression24_2 = stack[bandedValue24_2];
							if not cachedExpression24_2 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(bandedValue24_2) .. "]");
								cachedExpression24_2 = setValue(bandedValue24_2, (createDefaultMetadata(true)));
							end;
							evaluatedExpression24_2 = cachedExpression24_2;
						end;
						if expressionReuseCache[evaluatedExpression24_2] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[evaluatedExpression24_2] = true;
						end;
						v1516_9(v2833_0, v1624_2(l_v2833_1, unknownObject2, evaluatedExpression24_2));
					end, 
					IDIV = function(inputInteger3)
						local bandedValue8_3 = bit32.band(bit32.rshift(inputInteger3, 8), 255);
						local bandedValue16_3 = bit32.band(bit32.rshift(inputInteger3, 16), 255);
						local bandedValue24_3 = bit32.band(bit32.rshift(inputInteger3, 24), 255);
						local v1516_10 = processVariable;
						local v2844_0 = bandedValue8_3;
						local v1635_0 = createFloorDivisionOperation;
						local l_v2844_1 = bandedValue8_3;
						local evaluatedExpression16_3;
						if variableMap[bandedValue16_3] then
							evaluatedExpression16_3 = setValueWithConversion(bandedValue16_3, variableMap[bandedValue16_3]);
						else
							local cachedExpression16_3 = stack[bandedValue16_3];
							if not cachedExpression16_3 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(bandedValue16_3) .. "]");
								cachedExpression16_3 = setValue(bandedValue16_3, (createDefaultMetadata(true)));
							end;
							evaluatedExpression16_3 = cachedExpression16_3;
						end;
						if expressionReuseCache[evaluatedExpression16_3] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[evaluatedExpression16_3] = true;
						end;
						local l_v2851_0 = evaluatedExpression16_3;
						local evaluatedExpression24_3;
						if variableMap[bandedValue24_3] then
							evaluatedExpression24_3 = setValueWithConversion(bandedValue24_3, variableMap[bandedValue24_3]);
						else
							local expressionValue = stack[bandedValue24_3];
							if not expressionValue then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(bandedValue24_3) .. "]");
								expressionValue = setValue(bandedValue24_3, (createDefaultMetadata(true)));
							end;
							evaluatedExpression24_3 = expressionValue;
						end;
						if expressionReuseCache[evaluatedExpression24_3] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[evaluatedExpression24_3] = true;
						end;
						v1516_10(v2844_0, v1635_0(l_v2844_1, l_v2851_0, evaluatedExpression24_3));
					end, 
					IDIVK = function(combinedValue)
						local byte1 = bit32.band(bit32.rshift(combinedValue, 8), 255);
						local byte2 = bit32.band(bit32.rshift(combinedValue, 16), 255);
						local constantValue = constantsList[bit32.band(bit32.rshift(combinedValue, 24), 255)];
						if constantValue.type ~= 2 then
							addWarningComment(prefixWarning .. ": Malformed DIVK (constant wasn't a number)");
						end;
						local function1516 = processVariable;
						local byte1_0 = byte1;
						local function1635 = createFloorDivisionOperation;
						local l_v2857_1 = byte1;
						local evaluatedExpression;
						if variableMap[byte2] then
							evaluatedExpression = setValueWithConversion(byte2, variableMap[byte2]);
						else
							local expressionFallbackValue = stack[byte2];
							if not expressionFallbackValue then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(byte2) .. "]");
								expressionFallbackValue = setValue(byte2, (createDefaultMetadata(true)));
							end;
							evaluatedExpression = expressionFallbackValue;
						end;
						if expressionReuseCache[evaluatedExpression] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[evaluatedExpression] = true;
						end;
						function1516(byte1_0, function1635(l_v2857_1, evaluatedExpression, createConstant(nil, constantValue)));
					end, 
					POW = function(combinedValue2)
						local byte3 = bit32.band(bit32.rshift(combinedValue2, 8), 255);
						local byte4 = bit32.band(bit32.rshift(combinedValue2, 16), 255);
						local byte5 = bit32.band(bit32.rshift(combinedValue2, 24), 255);
						local function1516_2 = processVariable;
						local byte3_0 = byte3;
						local function1666 = createExponentiationOperation;
						local l_v2867_1 = byte3;
						local evaluatedExpression2;
						if variableMap[byte4] then
							evaluatedExpression2 = setValueWithConversion(byte4, variableMap[byte4]);
						else
							local expressionFallbackValue2 = stack[byte4];
							if not expressionFallbackValue2 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(byte4) .. "]");
								expressionFallbackValue2 = setValue(byte4, (createDefaultMetadata(true)));
							end;
							evaluatedExpression2 = expressionFallbackValue2;
						end;
						if expressionReuseCache[evaluatedExpression2] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[evaluatedExpression2] = true;
						end;
						local l_v2874_0 = evaluatedExpression2;
						local evaluatedExpression3;
						if variableMap[byte5] then
							evaluatedExpression3 = setValueWithConversion(byte5, variableMap[byte5]);
						else
							local expressionFallbackValue3 = stack[byte5];
							if not expressionFallbackValue3 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(byte5) .. "]");
								expressionFallbackValue3 = setValue(byte5, (createDefaultMetadata(true)));
							end;
							evaluatedExpression3 = expressionFallbackValue3;
						end;
						if expressionReuseCache[evaluatedExpression3] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[evaluatedExpression3] = true;
						end;
						function1516_2(byte3_0, function1666(l_v2867_1, l_v2874_0, evaluatedExpression3));
					end, 
					POWK = function(combinedValue3)
						local byte6 = bit32.band(bit32.rshift(combinedValue3, 8), 255);
						local byte7 = bit32.band(bit32.rshift(combinedValue3, 16), 255);
						local constantValue2 = constantsList[bit32.band(bit32.rshift(combinedValue3, 24), 255)];
						if constantValue2.type ~= 2 then
							addWarningComment(prefixWarning .. ": Malformed POWK (constant wasn't a number)");
						end;
						local function1516_3 = processVariable;
						local byte6_0 = byte6;
						local function1666_2 = createExponentiationOperation;
						local l_v2880_1 = byte6;
						local evaluatedExpression4;
						if variableMap[byte7] then
							evaluatedExpression4 = setValueWithConversion(byte7, variableMap[byte7]);
						else
							local expressionFallbackValue4 = stack[byte7];
							if not expressionFallbackValue4 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(byte7) .. "]");
								expressionFallbackValue4 = setValue(byte7, (createDefaultMetadata(true)));
							end;
							evaluatedExpression4 = expressionFallbackValue4;
						end;
						if expressionReuseCache[evaluatedExpression4] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[evaluatedExpression4] = true;
						end;
						function1516_3(byte6_0, function1666_2(l_v2880_1, evaluatedExpression4, createConstant(nil, constantValue2)));
					end, 
					MOD = function(combinedValue4)
						local byte8 = bit32.band(bit32.rshift(combinedValue4, 8), 255);
						local byte9 = bit32.band(bit32.rshift(combinedValue4, 16), 255);
						local bitmaskValue1 = bit32.band(bit32.rshift(combinedValue4, 24), 255);
						local function1 = processVariable;
						local function2 = byte8;
						local function3 = createModulusOperation;
						local l_v2890_1 = byte8;
						local expressionResult1;
						if variableMap[byte9] then
							expressionResult1 = setValueWithConversion(byte9, variableMap[byte9]);
						else
							local expressionResult2 = stack[byte9];
							if not expressionResult2 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(byte9) .. "]");
								expressionResult2 = setValue(byte9, (createDefaultMetadata(true)));
							end;
							expressionResult1 = expressionResult2;
						end;
						if expressionReuseCache[expressionResult1] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[expressionResult1] = true;
						end;
						local l_v2897_0 = expressionResult1;
						local expressionResult3;
						if variableMap[bitmaskValue1] then
							expressionResult3 = setValueWithConversion(bitmaskValue1, variableMap[bitmaskValue1]);
						else
							local expressionResult4 = stack[bitmaskValue1];
							if not expressionResult4 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(bitmaskValue1) .. "]");
								expressionResult4 = setValue(bitmaskValue1, (createDefaultMetadata(true)));
							end;
							expressionResult3 = expressionResult4;
						end;
						if expressionReuseCache[expressionResult3] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[expressionResult3] = true;
						end;
						function1(function2, function3(l_v2890_1, l_v2897_0, expressionResult3));
					end, 
					MODK = function(sourceValue1)
						local bitmaskValue2 = bit32.band(bit32.rshift(sourceValue1, 8), 255);
						local bitmaskValue3 = bit32.band(bit32.rshift(sourceValue1, 16), 255);
						local constantValue1 = constantsList[bit32.band(bit32.rshift(sourceValue1, 24), 255)];
						if constantValue1.type ~= 2 then
							addWarningComment(prefixWarning .. ": Malformed MODK (constant wasn't a number)");
						end;
						local function4 = processVariable;
						local bitmaskValue2_0 = bitmaskValue2;
						local function5 = createModulusOperation;
						local l_v2903_1 = bitmaskValue2;
						local expressionResult5;
						if variableMap[bitmaskValue3] then
							expressionResult5 = setValueWithConversion(bitmaskValue3, variableMap[bitmaskValue3]);
						else
							local expressionResult6 = stack[bitmaskValue3];
							if not expressionResult6 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(bitmaskValue3) .. "]");
								expressionResult6 = setValue(bitmaskValue3, (createDefaultMetadata(true)));
							end;
							expressionResult5 = expressionResult6;
						end;
						if expressionReuseCache[expressionResult5] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[expressionResult5] = true;
						end;
						function4(bitmaskValue2_0, function5(l_v2903_1, expressionResult5, createConstant(nil, constantValue1)));
					end, 
					AND = function(sourceValue2)
						local bitmaskValue4 = bit32.band(bit32.rshift(sourceValue2, 8), 255);
						local bitmaskValue5 = bit32.band(bit32.rshift(sourceValue2, 16), 255);
						local bitmaskValue6 = bit32.band(bit32.rshift(sourceValue2, 24), 255);
						local function6 = processVariable;
						local bitmaskValue4_0 = bitmaskValue4;
						local function7 = createOrExpression;
						local l_v2913_1 = bitmaskValue4;
						local expressionResult7;
						if variableMap[bitmaskValue5] then
							expressionResult7 = setValueWithConversion(bitmaskValue5, variableMap[bitmaskValue5]);
						else
							local expressionResult8 = stack[bitmaskValue5];
							if not expressionResult8 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(bitmaskValue5) .. "]");
								expressionResult8 = setValue(bitmaskValue5, (createDefaultMetadata(true)));
							end;
							expressionResult7 = expressionResult8;
						end;
						if expressionReuseCache[expressionResult7] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[expressionResult7] = true;
						end;
						local l_v2920_0 = expressionResult7;
						local expressionResult9;
						if variableMap[bitmaskValue6] then
							expressionResult9 = setValueWithConversion(bitmaskValue6, variableMap[bitmaskValue6]);
						else
							local expressionResult10 = stack[bitmaskValue6];
							if not expressionResult10 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(bitmaskValue6) .. "]");
								expressionResult10 = setValue(bitmaskValue6, (createDefaultMetadata(true)));
							end;
							expressionResult9 = expressionResult10;
						end;
						if expressionReuseCache[expressionResult9] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[expressionResult9] = true;
						end;
						function6(bitmaskValue4_0, function7(l_v2913_1, l_v2920_0, expressionResult9));
					end, 
					ANDK = function(sourceValue3)
						local bitmaskValue7 = bit32.band(bit32.rshift(sourceValue3, 8), 255);
						local bitmaskValue8 = bit32.band(bit32.rshift(sourceValue3, 16), 255);
						local constantValue1 = constantsList[bit32.band(bit32.rshift(sourceValue3, 24), 255)];
						local function8 = processVariable;
						local bitmaskValue7_0 = bitmaskValue7;
						local function9 = createOrExpression;
						local l_v2926_1 = bitmaskValue7;
						local expressionResult11;
						if variableMap[bitmaskValue8] then
							expressionResult11 = setValueWithConversion(bitmaskValue8, variableMap[bitmaskValue8]);
						else
							local evaluatedExpression = stack[bitmaskValue8];
							if not evaluatedExpression then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(bitmaskValue8) .. "]");
								evaluatedExpression = setValue(bitmaskValue8, (createDefaultMetadata(true)));
							end;
							expressionResult11 = evaluatedExpression;
						end;
						if expressionReuseCache[expressionResult11] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[expressionResult11] = true;
						end;
						function8(bitmaskValue7_0, function9(l_v2926_1, expressionResult11, createConstant(nil, constantValue1)));
					end, 
					OR = function(colorValue)
						local colorRed = bit32.band(bit32.rshift(colorValue, 8), 255);
						local colorGreen = bit32.band(bit32.rshift(colorValue, 16), 255);
						local colorBlue = bit32.band(bit32.rshift(colorValue, 24), 255);
						local callbackFunction1 = processVariable;
						local redValue = colorRed;
						local callbackFunction2 = createOrExpression;
						local l_v2936_1 = colorRed;
						local evaluatedColorGreen;
						if variableMap[colorGreen] then
							evaluatedColorGreen = setValueWithConversion(colorGreen, variableMap[colorGreen]);
						else
							local defaultColorGreen = stack[colorGreen];
							if not defaultColorGreen then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(colorGreen) .. "]");
								defaultColorGreen = setValue(colorGreen, (createDefaultMetadata(true)));
							end;
							evaluatedColorGreen = defaultColorGreen;
						end;
						if expressionReuseCache[evaluatedColorGreen] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[evaluatedColorGreen] = true;
						end;
						local l_v2943_0 = evaluatedColorGreen;
						local evaluatedColorBlue;
						if variableMap[colorBlue] then
							evaluatedColorBlue = setValueWithConversion(colorBlue, variableMap[colorBlue]);
						else
							local defaultColorBlue = stack[colorBlue];
							if not defaultColorBlue then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(colorBlue) .. "]");
								defaultColorBlue = setValue(colorBlue, (createDefaultMetadata(true)));
							end;
							evaluatedColorBlue = defaultColorBlue;
						end;
						if expressionReuseCache[evaluatedColorBlue] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[evaluatedColorBlue] = true;
						end;
						callbackFunction1(redValue, callbackFunction2(l_v2936_1, l_v2943_0, evaluatedColorBlue));
					end, 
					ORK = function(colorValue2)
						local colorRed2 = bit32.band(bit32.rshift(colorValue2, 8), 255);
						local colorGreen2 = bit32.band(bit32.rshift(colorValue2, 16), 255);
						local constantValue2 = constantsList[bit32.band(bit32.rshift(colorValue2, 24), 255)];
						local callbackFunction3 = processVariable;
						local redValue2 = colorRed2;
						local callbackFunction4 = createOrExpression;
						local l_v2949_1 = colorRed2;
						local evaluatedColorGreen2;
						if variableMap[colorGreen2] then
							evaluatedColorGreen2 = setValueWithConversion(colorGreen2, variableMap[colorGreen2]);
						else
							local defaultColorGreen2 = stack[colorGreen2];
							if not defaultColorGreen2 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(colorGreen2) .. "]");
								defaultColorGreen2 = setValue(colorGreen2, (createDefaultMetadata(true)));
							end;
							evaluatedColorGreen2 = defaultColorGreen2;
						end;
						if expressionReuseCache[evaluatedColorGreen2] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[evaluatedColorGreen2] = true;
						end;
						callbackFunction3(redValue2, callbackFunction4(l_v2949_1, evaluatedColorGreen2, createConstant(nil, constantValue2)));
					end, 
					CONCAT = function(colorValue3)
						local colorRed3 = bit32.band(bit32.rshift(colorValue3, 8), 255);
						local colorGreen3 = bit32.band(bit32.rshift(colorValue3, 16), 255);
						local colorBlue3 = bit32.band(bit32.rshift(colorValue3, 24), 255);
						local v2962 = {};
						for colorIndex = colorGreen3, colorBlue3 do
							local evaluatedColor;
							if variableMap[colorIndex] then
								evaluatedColor = setValueWithConversion(colorIndex, variableMap[colorIndex]);
							else
								local defaultColor = stack[colorIndex];
								if not defaultColor then
									addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(colorIndex) .. "]");
									defaultColor = setValue(colorIndex, (createDefaultMetadata(true)));
								end;
								evaluatedColor = defaultColor;
							end;
							if expressionReuseCache[evaluatedColor] then
								addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
							else
								expressionReuseCache[evaluatedColor] = true;
							end;
							table.insert(v2962, evaluatedColor);
						end;
						processVariable(colorRed3, createConcatenationExpression(colorRed3, v2962));
					end, 
					NOT = function(unknownValue12)
						local colorRed4 = bit32.band(bit32.rshift(unknownValue12, 8), 255);
						local colorGreen4 = bit32.band(bit32.rshift(unknownValue12, 16), 255);
						local callbackFunction5 = processVariable;
						local unknownValue1 = colorRed4;
						local unknownFunction1 = v1649;
						local l_v2967_1 = colorRed4;
						local expressionResult1;
						if variableMap[colorGreen4] then
							expressionResult1 = setValueWithConversion(colorGreen4, variableMap[colorGreen4]);
						else
							local expressionResult2 = stack[colorGreen4];
							if not expressionResult2 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(colorGreen4) .. "]");
								expressionResult2 = setValue(colorGreen4, (createDefaultMetadata(true)));
							end;
							expressionResult1 = expressionResult2;
						end;
						if expressionReuseCache[expressionResult1] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[expressionResult1] = true;
						end;
						callbackFunction5(unknownValue1, unknownFunction1(l_v2967_1, expressionResult1));
					end, 
					MINUS = function(unknownValue13)
						local colorComponentRed = bit32.band(bit32.rshift(unknownValue13, 8), 255);
						local colorComponentGreen = bit32.band(bit32.rshift(unknownValue13, 16), 255);
						local unknownFunction2 = processVariable;
						local colorComponentRedLocal = colorComponentRed;
						local unknownFunction3 = createNegateOperation;
						local l_v2976_1 = colorComponentRed;
						local expressionResult3;
						if variableMap[colorComponentGreen] then
							expressionResult3 = setValueWithConversion(colorComponentGreen, variableMap[colorComponentGreen]);
						else
							local expressionResult4 = stack[colorComponentGreen];
							if not expressionResult4 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(colorComponentGreen) .. "]");
								expressionResult4 = setValue(colorComponentGreen, (createDefaultMetadata(true)));
							end;
							expressionResult3 = expressionResult4;
						end;
						if expressionReuseCache[expressionResult3] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[expressionResult3] = true;
						end;
						unknownFunction2(colorComponentRedLocal, unknownFunction3(l_v2976_1, expressionResult3));
					end, 
					LENGTH = function(unknownValue14)
						local colorComponentBlue = bit32.band(bit32.rshift(unknownValue14, 8), 255);
						local colorComponentAlpha = bit32.band(bit32.rshift(unknownValue14, 16), 255);
						local unknownFunction4 = processVariable;
						local colorComponentBlueLocal = colorComponentBlue;
						local unknownFunction5 = createLengthOperation;
						local l_v2985_1 = colorComponentBlue;
						local expressionResult5;
						if variableMap[colorComponentAlpha] then
							expressionResult5 = setValueWithConversion(colorComponentAlpha, variableMap[colorComponentAlpha]);
						else
							local expressionResult6 = stack[colorComponentAlpha];
							if not expressionResult6 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(colorComponentAlpha) .. "]");
								expressionResult6 = setValue(colorComponentAlpha, (createDefaultMetadata(true)));
							end;
							expressionResult5 = expressionResult6;
						end;
						if expressionReuseCache[expressionResult5] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[expressionResult5] = true;
						end;
						unknownFunction4(colorComponentBlueLocal, unknownFunction5(l_v2985_1, expressionResult5));
					end, 
					NATIVECALL = function(_)
						local v2994 = linesList[#linesList];
						if v2994 and v2994.t == "comment" and string.sub(v2994.text, 1, 12) == "<NATIVECALL>" then
							v2994.stack = v2994.stack + 1;
							v2994.text = "<NATIVECALL>" .. " (x" .. v2994.stack .. ")";
						else
							lineData = createCommentObject("<NATIVECALL>");
							processUnknown();
						end;
						definePredefinedVariables();
					end, 
					BREAK = function(_)
						local v2996 = linesList[#linesList];
						if v2996 and v2996.t == "comment" and string.sub(v2996.text, 1, 12) == "<DEBUGBREAK>" then
							v2996.stack = v2996.stack + 1;
							v2996.text = "<DEBUGBREAK>" .. " (x" .. v2996.stack .. ")";
						else
							lineData = createCommentObject("<DEBUGBREAK>");
							processUnknown();
						end;
						definePredefinedVariables();
					end, 
					NOP = function(_)
						local v2998 = linesList[#linesList];
						if v2998 and v2998.t == "comment" and string.sub(v2998.text, 1, 5) == "<NOP>" then
							v2998.stack = v2998.stack + 1;
							v2998.text = "<NOP>" .. " (x" .. v2998.stack .. ")";
						else
							lineData = createCommentObject("<NOP>");
							processUnknown();
						end;
						definePredefinedVariables();
					end, 
					COVERAGE = function(_)
						local lastElement = linesList[#linesList];
						if lastElement and lastElement.t == "comment" and string.sub(lastElement.text, 1, 10) == "<COVERAGE>" then
							lastElement.stack = lastElement.stack + 1;
							lastElement.text = "<COVERAGE>" .. " (x" .. lastElement.stack .. ")";
						else
							lineData = createCommentObject("<COVERAGE>");
							processUnknown();
						end;
						definePredefinedVariables();
					end
				};
				local opcodeToFunctionMap = {};
				for opcodeIndex, opcodeFunction in pairs(opcodeHandlers) do
					local opcodeData = opcodeMapByName[opcodeIndex];
					if opcodeData then
						opcodeToFunctionMap[opcodeData.opcode] = opcodeFunction;
					end;
				end;
				local _ = {};
				local function processInstruction(v3006, dataList)
					local localV1203 = linesList;
					linesList = v3006;
					for _, instructionData in ipairs(dataList) do
						if instructionNumber < instructionData.index then
							instructionNumber = instructionData.index;
							instructionIndex = instructionData.code_index;
							local instruction = instructionList[instructionNumber];
							local instructionHandler = opcodeToFunctionMap[instruction.opcode];
							if instructionHandler then
								lockUnlockVariables();
								instructionHandler(instruction.inst);
								if not success then
									error(instruction.opname);
								end;
							else
								addWarningComment((("%*: Skipped instruction %*"):format(prefixWarning, instruction.opname)));
							end;
						end;
					end;
					linesList = localV1203;
				end;
				local evaluateCondition = nil;
				local function jumpHandler(_, _)
					definePredefinedVariables();
				end;
				local function jumpIfHandler(instructionData, _)
					local colorComponentRed2 = bit32.band(bit32.rshift(instructionData, 8), 255);
					local unknownValue2 = codeIndexMap[instructionIndex];
					local unknownFunction6 = addDependency;
					local unknownValue = unknownValue2;
					local expressionResult;
					if variableMap[colorComponentRed2] then
						expressionResult = setValueWithConversion(colorComponentRed2, variableMap[colorComponentRed2]);
					else
						local evaluatedExpression = stack[colorComponentRed2];
						if not evaluatedExpression then
							addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(colorComponentRed2) .. "]");
							evaluatedExpression = setValue(colorComponentRed2, (createDefaultMetadata(true)));
						end;
						expressionResult = evaluatedExpression;
					end;
					if expressionReuseCache[expressionResult] then
						addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
					else
						expressionReuseCache[expressionResult] = true;
					end;
					unknownFunction6(unknownValue, expressionResult);
					definePredefinedVariables();
				end;
				local function jumpIfCompareHandler(instructionData2, _)
					local byteValue1 = bit32.band(bit32.rshift(instructionData2, 8), 255);
					local auxiliaryValue1 = codeList[instructionIndex + 1];
					if not auxiliaryValue1 then
						error("Expected aux");
					end;
					local localAuxiliaryValue1 = auxiliaryValue1;
					auxiliaryValue1 = codeIndexMap[instructionIndex];
					local unknownFunction2 = addDependency;
					local localAuxiliaryValue2 = auxiliaryValue1;
					local expressionResult2;
					if variableMap[byteValue1] then
						expressionResult2 = setValueWithConversion(byteValue1, variableMap[byteValue1]);
					else
						local evaluatedExpression2 = stack[byteValue1];
						if not evaluatedExpression2 then
							addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(byteValue1) .. "]");
							evaluatedExpression2 = setValue(byteValue1, (createDefaultMetadata(true)));
						end;
						expressionResult2 = evaluatedExpression2;
					end;
					if expressionReuseCache[expressionResult2] then
						addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
					else
						expressionReuseCache[expressionResult2] = true;
					end;
					unknownFunction2(localAuxiliaryValue2, expressionResult2);
					unknownFunction2 = addDependency;
					localAuxiliaryValue2 = auxiliaryValue1;
					if variableMap[localAuxiliaryValue1] then
						expressionResult2 = setValueWithConversion(localAuxiliaryValue1, variableMap[localAuxiliaryValue1]);
					else
						local evaluatedExpression3 = stack[localAuxiliaryValue1];
						if not evaluatedExpression3 then
							addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(localAuxiliaryValue1) .. "]");
							evaluatedExpression3 = setValue(localAuxiliaryValue1, (createDefaultMetadata(true)));
						end;
						expressionResult2 = evaluatedExpression3;
					end;
					if expressionReuseCache[expressionResult2] then
						addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
					else
						expressionReuseCache[expressionResult2] = true;
					end;
					unknownFunction2(localAuxiliaryValue2, expressionResult2);
					definePredefinedVariables();
				end;
				local function jumpXEqHandler(instructionData3, _)
					local byteValue2 = bit32.band(bit32.rshift(instructionData3, 8), 255);
					if not codeList[instructionIndex + 1] then
						error("Expected aux");
					end;
					local auxiliaryValue2 = codeIndexMap[instructionIndex];
					local unknownFunction3 = addDependency;
					local localAuxiliaryValue3 = auxiliaryValue2;
					local expressionResult3;
					if variableMap[byteValue2] then
						expressionResult3 = setValueWithConversion(byteValue2, variableMap[byteValue2]);
					else
						local evaluatedExpression4 = stack[byteValue2];
						if not evaluatedExpression4 then
							addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(byteValue2) .. "]");
							evaluatedExpression4 = setValue(byteValue2, (createDefaultMetadata(true)));
						end;
						expressionResult3 = evaluatedExpression4;
					end;
					if expressionReuseCache[expressionResult3] then
						addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
					else
						expressionReuseCache[expressionResult3] = true;
					end;
					unknownFunction3(localAuxiliaryValue3, expressionResult3);
					definePredefinedVariables();
				end;
				local jumpTable = {
					JUMP = jumpHandler, 
					JUMPX = jumpHandler, 
					JUMPBACK = jumpHandler, 
					JUMPIF = jumpIfHandler, 
					JUMPIFNOT = jumpIfHandler, 
					JUMPIFEQ = jumpIfCompareHandler, 
					JUMPIFLE = jumpIfCompareHandler, 
					JUMPIFLT = jumpIfCompareHandler, 
					JUMPIFNOTEQ = jumpIfCompareHandler, 
					JUMPIFNOTLE = jumpIfCompareHandler, 
					JUMPIFNOTLT = jumpIfCompareHandler, 
					JUMPXEQKNIL = jumpXEqHandler, 
					JUMPXEQKB = jumpXEqHandler, 
					JUMPXEQKN = jumpXEqHandler, 
					JUMPXEQKS = jumpXEqHandler, 
					FORNLOOP = function(v3047, _)
						local byteValue3 = bit32.band(bit32.rshift(v3047, 8), 255);
						local auxiliaryValue3 = codeIndexMap[instructionIndex];
						local unknownFunction4 = addDependency;
						local localAuxiliaryValue4 = auxiliaryValue3;
						local expressionResult4;
						if variableMap[byteValue3] then
							expressionResult4 = setValueWithConversion(byteValue3, variableMap[byteValue3]);
						else
							local evaluatedExpression5 = stack[byteValue3];
							if not evaluatedExpression5 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(byteValue3) .. "]");
								evaluatedExpression5 = setValue(byteValue3, (createDefaultMetadata(true)));
							end;
							expressionResult4 = evaluatedExpression5;
						end;
						if expressionReuseCache[expressionResult4] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[expressionResult4] = true;
						end;
						unknownFunction4(localAuxiliaryValue4, expressionResult4);
						unknownFunction4 = addDependency;
						localAuxiliaryValue4 = auxiliaryValue3;
						local byteValueIncremented = byteValue3 + 1;
						if variableMap[byteValueIncremented] then
							expressionResult4 = setValueWithConversion(byteValueIncremented, variableMap[byteValueIncremented]);
						else
							local evaluatedExpression6 = stack[byteValueIncremented];
							if not evaluatedExpression6 then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(byteValueIncremented) .. "]");
								evaluatedExpression6 = setValue(byteValueIncremented, (createDefaultMetadata(true)));
							end;
							expressionResult4 = evaluatedExpression6;
						end;
						if expressionReuseCache[expressionResult4] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[expressionResult4] = true;
						end;
						unknownFunction4(localAuxiliaryValue4, expressionResult4);
						definePredefinedVariables();
					end, 
					FORGLOOP = function(v3057, value)
						local byteValue4 = bit32.band(bit32.rshift(v3057, 8), 255);
						local auxiliaryValue4 = codeIndexMap[instructionIndex];
						local unknownFunction5 = addDependency;
						local localAuxiliaryValue5 = auxiliaryValue4;
						local expressionValue;
						if variableMap[byteValue4] then
							expressionValue = setValueWithConversion(byteValue4, variableMap[byteValue4]);
						else
							local cachedExpressionValue = stack[byteValue4];
							if not cachedExpressionValue then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(byteValue4) .. "]");
								cachedExpressionValue = setValue(byteValue4, (createDefaultMetadata(true)));
							end;
							expressionValue = cachedExpressionValue;
						end;
						if expressionReuseCache[expressionValue] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[expressionValue] = true;
						end;
						unknownFunction5(localAuxiliaryValue5, expressionValue);
						unknownFunction5 = addDependency;
						localAuxiliaryValue5 = auxiliaryValue4;
						local expressionIndex = byteValue4 + 1;
						if variableMap[expressionIndex] then
							expressionValue = setValueWithConversion(expressionIndex, variableMap[expressionIndex]);
						else
							local expressionResult = stack[expressionIndex];
							if not expressionResult then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(expressionIndex) .. "]");
								expressionResult = setValue(expressionIndex, (createDefaultMetadata(true)));
							end;
							expressionValue = expressionResult;
						end;
						if expressionReuseCache[expressionValue] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[expressionValue] = true;
						end;
						unknownFunction5(localAuxiliaryValue5, expressionValue);
						unknownFunction5 = addDependency;
						localAuxiliaryValue5 = auxiliaryValue4;
						expressionIndex = byteValue4 + 2;
						if variableMap[expressionIndex] then
							expressionValue = setValueWithConversion(expressionIndex, variableMap[expressionIndex]);
						else
							local expressionResultAlternative = stack[expressionIndex];
							if not expressionResultAlternative then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(expressionIndex) .. "]");
								expressionResultAlternative = setValue(expressionIndex, (createDefaultMetadata(true)));
							end;
							expressionValue = expressionResultAlternative;
						end;
						if expressionReuseCache[expressionValue] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[expressionValue] = true;
						end;
						unknownFunction5(localAuxiliaryValue5, expressionValue);
						assert(value);
						for _ = 1, bit32.band(value, 255) do

						end;
						definePredefinedVariables();
					end
				};
				local function processControlFlowBlock(controlFlowBlock, controlFlowBlockData, isFirstBlock)
					local previousControlFlowBlock = nil;
					if isFirstBlock then
						previousControlFlowBlock = linesList;
						linesList = controlFlowBlock;
						index = index + 1;
					end;
					local isSingleType = false;
					if controlFlowBlockData.analysis_failed then
						if controlFlowBlockData.type == "single" and #controlFlowBlockData.data == 0 and not controlFlowBlockData.jump then
							isSingleType = true;
						else
							addWarningComment((("%*: [%*] %*. Error Block %* start (CF ANALYSIS FAILED)"):format(prefixErrorValue, controlFlowBlockData.actual_code_index, controlFlowBlockData.actual_index, controlFlowBlockData.hl_index)));
						end;
					end;
					if not isSingleType then
						if controlFlowBlockData.type == "single" then
							processInstruction(controlFlowBlock, controlFlowBlockData.data);
							local bytecodeVisitor = controlFlowBlockData._visitor;
							if bytecodeVisitor then
								local bytecodeInstruction = bytecodeVisitor.code;
								local bytecodeHandler = jumpTable[bytecodeInstruction.opname];
								if bytecodeHandler then
									lockUnlockVariables();
									bytecodeHandler(bytecodeInstruction.inst, bytecodeInstruction.aux);
								end;
							end;
						elseif controlFlowBlockData.type == "oneblockwhile" then
							local controlFlowBlocks = {};
							local bytecodeData = controlFlowBlockData.data;
							local loopBlock = nil;
							local forLoopInfo = bytecodeData.for_info;
							if forLoopInfo then
								loopBlock = {
									t = "for", 
									lines = linesList, 
									reads = {}, 
									writes = {}, 
									code = controlFlowBlocks, 
									for_info = forLoopInfo
								};
								local loopVariables = forLoopInfo.variables;
								assert(loopVariables);
								for variableIndex, loopVariable in ipairs(loopVariables) do
									loopVariable.init_expr = loopBlock;
									loopVariable.var_num = variableIndex;
									local variableWriteInfo = loopBlock;
									table.insert(variableWriteInfo.writes, loopVariable);
									table.insert(loopVariable.writes, variableWriteInfo);
								end;
								local loopType = forLoopInfo.type;
								if loopType == "numeric" then
									local loopArguments = forLoopInfo.args;
									local loopIndexRegister = loopArguments.index_reg;
									local loopIndexValue;
									if variableMap[loopIndexRegister] then
										loopIndexValue = setValueWithConversion(loopIndexRegister, variableMap[loopIndexRegister]);
									else
										local loopIndexDefaultValue = stack[loopIndexRegister];
										if not loopIndexDefaultValue then
											addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(loopIndexRegister) .. "]");
											loopIndexDefaultValue = setValue(loopIndexRegister, (createDefaultMetadata(true)));
										end;
										loopIndexValue = loopIndexDefaultValue;
									end;
									loopArguments.index_expr = loopIndexValue;
									assert(loopArguments.index_expr);
									addDependency(loopBlock, loopArguments.index_expr);
									loopIndexRegister = loopArguments.end_reg;
									if variableMap[loopIndexRegister] then
										loopIndexValue = setValueWithConversion(loopIndexRegister, variableMap[loopIndexRegister]);
									else
										local loopEndValue = stack[loopIndexRegister];
										if not loopEndValue then
											addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(loopIndexRegister) .. "]");
											loopEndValue = setValue(loopIndexRegister, (createDefaultMetadata(true)));
										end;
										loopIndexValue = loopEndValue;
									end;
									loopArguments.end_expr = loopIndexValue;
									assert(loopArguments.end_expr);
									addDependency(loopBlock, loopArguments.end_expr);
									loopIndexRegister = loopArguments.step_reg;
									if variableMap[loopIndexRegister] then
										loopIndexValue = setValueWithConversion(loopIndexRegister, variableMap[loopIndexRegister]);
									else
										local loopStepValue = stack[loopIndexRegister];
										if not loopStepValue then
											addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(loopIndexRegister) .. "]");
											loopStepValue = setValue(loopIndexRegister, (createDefaultMetadata(true)));
										end;
										loopIndexValue = loopStepValue;
									end;
									loopArguments.step_expr = loopIndexValue;
									assert(loopArguments.step_expr);
									addDependency(loopBlock, loopArguments.step_expr);
									assert(#loopVariables == 1);
									variableMap[loopArguments.index_reg] = loopVariables[1];
								elseif loopType == "generic" then
									local generatorArguments = forLoopInfo.args;
									local generatorRegister = generatorArguments.generator_reg;
									local generatorValue;
									if variableMap[generatorRegister] then
										generatorValue = setValueWithConversion(generatorRegister, variableMap[generatorRegister]);
									else
										local generatorDefaultValue = stack[generatorRegister];
										if not generatorDefaultValue then
											addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(generatorRegister) .. "]");
											generatorDefaultValue = setValue(generatorRegister, (createDefaultMetadata(true)));
										end;
										generatorValue = generatorDefaultValue;
									end;
									generatorArguments.generator_expr = generatorValue;
									generatorRegister = generatorArguments.state_reg;
									if variableMap[generatorRegister] then
										generatorValue = setValueWithConversion(generatorRegister, variableMap[generatorRegister]);
									else
										local generatorStateValue = stack[generatorRegister];
										if not generatorStateValue then
											addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(generatorRegister) .. "]");
											generatorStateValue = setValue(generatorRegister, (createDefaultMetadata(true)));
										end;
										generatorValue = generatorStateValue;
									end;
									generatorArguments.state_expr = generatorValue;
									generatorRegister = generatorArguments.index_reg;
									if variableMap[generatorRegister] then
										generatorValue = setValueWithConversion(generatorRegister, variableMap[generatorRegister]);
									else
										local generatorIndexValue = stack[generatorRegister];
										if not generatorIndexValue then
											addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(generatorRegister) .. "]");
											generatorIndexValue = setValue(generatorRegister, (createDefaultMetadata(true)));
										end;
										generatorValue = generatorIndexValue;
									end;
									generatorArguments.index_expr = generatorValue;
									for loopVariableIndex, loopVariable in ipairs(loopVariables) do
										assert(loopVariable.var_num == loopVariableIndex);
										variableMap[forLoopInfo.variables_reg_range.beginning + loopVariableIndex - 1] = loopVariable;
									end;
								else
									error((("Unknown for loop type %*"):format(loopType)));
								end;
								local bytecodeVisitor = controlFlowBlockData._visitor;
								assert(bytecodeVisitor);
								local instructionTable = codeIndexMap[bytecodeVisitor.code.code_index];
								local dataMap = loopBlock;
								table.clear(instructionTable);
								for key, value in pairs(dataMap) do
									instructionTable[key] = value;
								end;
								processControlFlowBlock(controlFlowBlocks, jumpTable[bytecodeData.code], true);
							else
								loopBlock = {
									t = "while", 
									lines = linesList, 
									reads = {}, 
									writes = {}, 
									code = controlFlowBlocks, 
									expr = nil
								};
								processControlFlowBlock(controlFlowBlocks, jumpTable[bytecodeData.code], true);
							end;
							for _, childStatement in ipairs(controlFlowBlocks) do
								childStatement.parent = loopBlock;
							end;
							table.insert(linesList, loopBlock);
						elseif controlFlowBlockData.type == "sequential" then
							for _, dataItem in ipairs(controlFlowBlockData.data) do
								processControlFlowBlock(controlFlowBlock, jumpTable[dataItem]);
							end;
						elseif not (controlFlowBlockData.type ~= "ifthen") or controlFlowBlockData.type == "ifthenelse" then
							local data = controlFlowBlockData.data;
							local conditionMet = true;
							local condition = data.condition;
							while true do
								if condition.type == "and" then
									local conditionLeftHandSide = condition.lhs;
									if type(conditionLeftHandSide) == "table" and conditionLeftHandSide.type ~= "reg" then
										conditionMet = nil;
										break;
									else
										local conditionRightHandSide = condition.rhs;
										if type(conditionRightHandSide) == "table" then
											if conditionRightHandSide.type == "and" then
												condition = conditionRightHandSide;
											elseif not (conditionRightHandSide.type == "reg") then
												conditionMet = nil;
												break;
											else
												break;
											end;
										else
											break;
										end;
									end;
								else
									conditionMet = nil;
									break;
								end;
							end;
							local ifConditionExpression = evaluateCondition(controlFlowBlock, data.condition, conditionMet, true);
							local passStatements = {};
							processControlFlowBlock(passStatements, jumpTable[data.pass], true);
							local elseStatements = nil;
							if data.else_ then
								elseStatements = {};
								processControlFlowBlock(elseStatements, jumpTable[data.else_], true);
							end;
							local ifStatement = {
								t = "if", 
								lines = linesList, 
								reads = {}, 
								writes = {}, 
								pass = passStatements, 
								elseifs = {}, 
								else_ = elseStatements, 
								expr = ifConditionExpression
							};
							for _, passStatement in ipairs(passStatements) do
								passStatement.parent = ifStatement;
							end;
							if elseStatements then
								for _, elseStatement in ipairs(elseStatements) do
									elseStatement.parent = ifStatement;
								end;
							end;
							table.insert(linesList, ifStatement);
						else
							error((("Unknown hl_block type \"%*\""):format(controlFlowBlockData.type)));
						end;
						local jumpInfo = controlFlowBlockData.jump;
						if jumpInfo then
							local jumpType = jumpInfo.type;
							if jumpType == "break" then
								breakStatementFunction();
							elseif jumpType == "continue" then
								continueStatement();
							elseif jumpType == "goto" then
								unknownJumpFunction(jumpInfo.destination);
							else
								error((("Unknown jump_type \"%*\""):format(jumpType)));
							end;
							processUnknown();
						end;
						if controlFlowBlockData.analysis_failed then
							addWarningComment((("%*: [%*] %*. Error Block %* end (CF ANALYSIS FAILED)"):format(prefixErrorValue, codeIndexToActualIndexMap[controlFlowBlockData.code_index], indexMap[controlFlowBlockData.index], controlFlowBlockData.hl_index)));
						end;
					end;
					if isFirstBlock then
						linesList = previousControlFlowBlock;
						index = index - 1;
					end;
				end;
				evaluateCondition = function(environment, expression, conditionResult, isFullCondition)
					if expression.type == "reg" then
						assert(expression.code);
						assert(type(expression.lhs) == "number");
						local startIndex = #linesList;
						local lastElement = linesList[#linesList];
						processControlFlowBlock(environment, jumpTable[expression.code]);
						local lhsValue;
						if type(expression.lhs) == "number" then
							local lhs = expression.lhs;
							if variableMap[lhs] then
								lhsValue = setValueWithConversion(lhs, variableMap[lhs]);
							else
								local lhsResolved = stack[lhs];
								if not lhsResolved then
									addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(lhs) .. "]");
									lhsResolved = setValue(lhs, (createDefaultMetadata(true)));
								end;
								lhsValue = lhsResolved;
							end;
						else
							lhsValue = expression.lhs;
						end;
						local rhsValue = nil;
						if expression.rhs then
							if type(expression.rhs) == "number" then
								local rhs = expression.rhs;
								if variableMap[rhs] then
									rhsValue = setValueWithConversion(rhs, variableMap[rhs]);
								else
									local rhsResolved = stack[rhs];
									if not rhsResolved then
										addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(rhs) .. "]");
										rhsResolved = setValue(rhs, (createDefaultMetadata(true)));
									end;
									rhsValue = rhsResolved;
								end;
							else
								rhsValue = expression.rhs;
							end;
						end;
						if not isFullCondition and startIndex < #linesList and lastElement ~= linesList[#linesList] then
							if conditionResult then
								conditionStopPointsMap[jumpedToByMap[jumpTable[expression.code].index].index] = true;
								markedConditionStopPointsCount = markedConditionStopPointsCount + 1;
							else
								local functionAst = {};
								while startIndex < #linesList do
									local removedStatement = table.remove(linesList);
									assert(removedStatement);
									local removedLine = removedStatement;
									removedLine.lines = functionAst;
									table.insert(functionAst, removedLine);
								end;
								local lhsValueFinal = lhsValue;
								if rhsValue then
									local conditionalExpression = lhsValue;
									local condition = expression.condition;
									lhsValueFinal = setValue(nil, (createConditionMetadata(conditionalExpression, condition, rhsValue)));
									expression.condition = "exist";
									expression.rhs = nil;
									rhsValue = nil;
								end;
								table.insert(functionAst, (createReturnMetadata(functionAst, {
									lhsValueFinal
								})));
								local inlinedVarName = variableIndexMap.INLINED;
								local inlinedFunctionName = "INLINED";
								local inlinedFunctionCounter = inlinedVarName or 1;
								while variableMap[inlinedFunctionName] or globalNameCache[inlinedFunctionName] do
									inlinedFunctionCounter = inlinedFunctionCounter + 1;
									inlinedFunctionName = "INLINED" .. "_" .. inlinedFunctionCounter;
								end;
								variableIndexMap.INLINED = inlinedFunctionCounter;
								local inlinedFunctionNameBase = inlinedFunctionName;
								inlinedVarName = allocateVariable(inlinedFunctionNameBase, {
									beginning = -1, 
									ending = -1
								}, {});
								lhsValue = combineMetadata(setValueWithConversion(nil, inlinedVarName), {}, false);
								inlinedFunctionName = {
									t = "function", 
									reads = {}, 
									writes = {}, 
									contributors = {}, 
									is_self_referencing = false, 
									name = inlinedFunctionNameBase, 
									varname = inlinedVarName, 
									name_known = true, 
									args = {}, 
									is_vararg = false, 
									line_defined = -1, 
									upvalues_count = 0, 
									upvalues = {}, 
									ast = functionAst
								};
								table.insert(inlinedFunctionName.writes, inlinedVarName);
								table.insert(linesList, (createDefineFunctionMetadata(linesList, inlinedFunctionName, inlinedVarName, "local")));
							end;
						end;
						expression.lhs = lhsValue;
						expression.rhs = rhsValue;
					end;
					local lhsResult = nil;
					local lhsExpression = expression.lhs;
					assert(lhsExpression);
					if type(lhsExpression) == "number" then
						local lhsResolvedValue;
						if variableMap[lhsExpression] then
							lhsResolvedValue = setValueWithConversion(lhsExpression, variableMap[lhsExpression]);
						else
							local lhsDefaultValue = stack[lhsExpression];
							if not lhsDefaultValue then
								addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(lhsExpression) .. "]");
								lhsDefaultValue = setValue(lhsExpression, (createDefaultMetadata(true)));
							end;
							lhsResolvedValue = lhsDefaultValue;
						end;
						if expressionReuseCache[lhsResolvedValue] then
							addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
						else
							expressionReuseCache[lhsResolvedValue] = true;
						end;
						lhsResult = lhsResolvedValue;
						error("messgage");
					else
						lhsResult = if lhsExpression.is_full_condition_t then evaluateCondition(environment, lhsExpression, conditionResult, isFullCondition) else lhsExpression;
					end;
					expression.lhs = lhsResult;
					local rhsResult = nil;
					local rhsValue = expression.rhs;
					if rhsValue then
						if type(rhsValue) == "number" then
							local evaluatedValue;
							if variableMap[rhsValue] then
								evaluatedValue = setValueWithConversion(rhsValue, variableMap[rhsValue]);
							else
								local cachedValue = stack[rhsValue];
								if not cachedValue then
									addWarningComment(prefixWarning .. ": Failed to evaluate expression, replaced with nil [" .. tostring(instructionIndex) .. "." .. tostring(rhsValue) .. "]");
									cachedValue = setValue(rhsValue, (createDefaultMetadata(true)));
								end;
								evaluatedValue = cachedValue;
							end;
							if expressionReuseCache[evaluatedValue] then
								addWarningComment(prefixErrorValue .. ": Expression was reused, decompilation is incorrect");
							else
								expressionReuseCache[evaluatedValue] = true;
							end;
							rhsResult = evaluatedValue;
							error("messagee");
						else
							rhsResult = if rhsValue.is_full_condition_t then evaluateCondition(environment, rhsValue, conditionResult) else rhsValue;
						end;
					end;
					if expression.type ~= "reg" then
						assert(rhsResult);
						if expression.type == "and" then
							lhsResult = setValue(nil, (createAndMetadata(lhsResult, rhsResult)));
						elseif expression.type == "or" then
							lhsResult = createOrExpression(nil, lhsResult, rhsResult);
						else
							error("Impossible!!!!!!!!!!!!!!!!!!!!!!!!!");
						end;
						rhsResult = nil;
					end;
					if rhsResult then
						return createAndExpression(nil, lhsResult, expression.condition, rhsResult);
					elseif expression.condition == "not exist" then
						return v1649(nil, lhsResult);
					else
						return lhsResult;
					end;
				end;
				processControlFlowBlock(linesList, controlFlowEntries());
				for _, constant in constantsList do
					if constant.type == 4 and type(constant.value) == "number" then
						constant.value = processConstant(nil, constant.value);
					end;
				end;
				benchmark:end_benchmark("AST Generation");
				unknownValue = localValue;
				benchmark:print_all_times();
				return linesList;
			end;
		end;
		protectedCall = function(functionToGenerateAst, argument)
			local astGeneratorState = createAnalysisData();
			local success, errorMessage = xpcall(executeProto, function(errorMessage)
				return tostring(errorMessage) .. "\nTraceback:\n" .. debug.traceback(nil, 2);
			end, functionToGenerateAst, argument);
			local v1338 = conditionStopPointsMap;
			if success then
				if markedConditionStopPointsCount > astGeneratorState.marked_condition_stop_points then
					globalFailedInstructionsCount = astGeneratorState.global_failed_instructions_count;
					noticesList = astGeneratorState.notices;
					linesList = astGeneratorState.lines;
					variableMappedLongStringConstants = astGeneratorState.variable_mapped_long_string_constants;
					variableMappedLongStringConstantsOrder = astGeneratorState.variable_mapped_long_string_constants_order;
					longStringUsageCounts = astGeneratorState.long_string_usage_counts;
					longStringConstantAlreadyUsedMap = astGeneratorState.long_string_constant_already_used;
					longStringVariableCount = astGeneratorState.long_string_variable_count;
					linesHadSkippedReturnMap = astGeneratorState.lines_had_skipped_return;
					conditionStopPointsMap = astGeneratorState.condition_stop_points;
					markedConditionStopPointsCount = astGeneratorState.marked_condition_stop_points;
					conditionStopPointsMap = v1338;
					return protectedCall(functionToGenerateAst, argument);
				else
					return errorMessage;
				end;
			else
				globalFailedInstructionsCount = astGeneratorState.global_failed_instructions_count;
				noticesList = astGeneratorState.notices;
				linesList = astGeneratorState.lines;
				variableMappedLongStringConstants = astGeneratorState.variable_mapped_long_string_constants;
				variableMappedLongStringConstantsOrder = astGeneratorState.variable_mapped_long_string_constants_order;
				longStringUsageCounts = astGeneratorState.long_string_usage_counts;
				longStringConstantAlreadyUsedMap = astGeneratorState.long_string_constant_already_used;
				longStringVariableCount = astGeneratorState.long_string_variable_count;
				linesHadSkippedReturnMap = astGeneratorState.lines_had_skipped_return;
				conditionStopPointsMap = astGeneratorState.condition_stop_points;
				markedConditionStopPointsCount = astGeneratorState.marked_condition_stop_points;
				if functionToGenerateAst.debug_name then
					return {
						(createCommentObject(prefixErrorValue .. ": Failed to generate AST for function `" .. tostring(functionToGenerateAst.debug_name) .. "`:\n" .. tostring(errorMessage)))
					};
				else
					return {
						(createCommentObject(prefixErrorValue .. ": Failed to generate AST for unnamed function:\n" .. tostring(errorMessage)))
					};
				end;
			end;
		end;
		benchmarkTimer:end_benchmark("Global Initialization");
		benchmarkTimer:start_benchmark("Global Mapping");
		for _, bytecodeChunk in pairs(bytecodeConstants) do
			local instructionIndex = 0;
			local bytecode = bytecodeChunk.code;
			local bytecodeLength = #bytecode;
			while instructionIndex < bytecodeLength do
				local instructionData = opcodeMapByCode[bit32.band(bytecode[instructionIndex], 255)];
				if instructionData then
					if instructionData.opname == "SETGLOBAL" then
						local constantIndex = bytecode[instructionIndex + 1];
						local constantValue = bytecodeChunk.constants[constantIndex];
						if constantValue then
							if constantValue.type == 3 then
								globalNameCache[constantValue.value] = true;
							else
								local constantType = dataTypeNames[constantValue.type];
								if constantType then
									local warningMessage = {
										type = "warning", 
										content = ("A SETGLOBAL instruction specified a%* %* constant. Expected a string."):format(vowelMap[string.sub(constantType, 1, 1)] and "n" or "", constantType)
									};
									table.insert(noticesList, warningMessage);
								else
									table.insert(noticesList, {
										type = "warning", 
										content = "A SETGLOBAL instruction specified a constant with an invalid type."
									});
								end;
							end;
						else
							table.insert(noticesList, {
								type = "warning", 
								content = "A SETGLOBAL instruction specified an invalid constant."
							});
						end;
					end;
					instructionIndex = instructionIndex + instructionData.size;
				else
					instructionIndex = instructionIndex + 1;
				end;
			end;
		end;
		benchmarkTimer:end_benchmark("Global Mapping");
		benchmarkTimer:start_benchmark("Global Decompilation");
		local astNodes = protectedCall(bytecodeChunk);
		benchmarkTimer:end_benchmark("Global Decompilation");
		benchmarkTimer:start_benchmark("Global High Level Reductions");
		local counter = nil;
		local function _()
			counter = counter + 1;
		end;
		local processLine = nil;
		local processExpression = nil;
		local lineIndex = nil;
		local function processBinaryExpression(binaryExpression)
			processExpression(binaryExpression.lhs);
			if binaryExpression.rhs then
				processExpression(binaryExpression.rhs);
			end;
		end;
		local function processNegateExpression(negateExpression)
			processExpression(negateExpression.rhs);
		end;
		local function emptyFunction(_)

		end;
		local processedNodes = nil;
		local expressionProcessors = {
			["nil"] = emptyFunction, 
			boolean = emptyFunction, 
			global = emptyFunction, 
			varargs = emptyFunction, 
			addition = processBinaryExpression, 
			subtraction = processBinaryExpression, 
			multiplication = processBinaryExpression, 
			division = processBinaryExpression, 
			exponentiation = processBinaryExpression, 
			["floor division"] = processBinaryExpression, 
			modulus = processBinaryExpression, 
			["and"] = processBinaryExpression, 
			["or"] = processBinaryExpression, 
			condition = processBinaryExpression, 
			negate = processNegateExpression, 
			["not"] = function(notExpression)
				local notCount = 1;
				local currentNotExpression = notExpression;
				while currentNotExpression.rhs.t == "not" do
					notCount = notCount + 1;
					currentNotExpression = currentNotExpression.rhs;
				end;
				local notRhs = currentNotExpression.rhs;
				if notCount > 2 then
					if notCount % 2 == 0 then
						notExpression.rhs.rhs = notRhs;
					else
						notExpression.rhs = notRhs;
					end;
				end;
				processExpression(notExpression.rhs);
			end, 
			length = processNegateExpression, 
			concatenation = function(expressionList)
				for _, expression in ipairs(expressionList.exprs) do
					processExpression(expression);
				end;
			end, 
			name = function(v3196)
				local overrideExpression = v3196.name.override_expr;
				if overrideExpression then
					processExpression(overrideExpression);
				end;
			end, 
			constant = function(v3198)
				local constantValue = v3198.const;
				if constantValue.type == 4 then
					processExpression(constantValue.value);
				end;
			end, 
			["constant index"] = function(initializerData)
				local indexValue = initializerData.index;
				if indexValue.type == 4 then
					processExpression(indexValue.value);
				end;
				processExpression(initializerData.table);
			end, 
			["new table"] = function(initializerTable)
				for initializerKey, initializerValue in pairs(initializerTable.initializers) do
					if type(initializerKey) == "table" then
						processExpression(initializerKey);
						processExpression(initializerValue);
					else
						processExpression(initializerValue);
					end;
				end;
			end, 
			["function"] = function(v3205)
				processLine(v3205.ast);
			end, 
			call = function(functionData)
				processExpression(functionData.func);
				for _, argument in ipairs(functionData.args) do
					processExpression(argument);
				end;
			end, 
			["get table"] = function(v3209)
				processExpression(v3209.table);
				processExpression(v3209.index);
			end
		};
		processExpression = function(expressionNode)
			if processedNodes[expressionNode] then
				return;
			else
				processedNodes[expressionNode] = true;
				local expressionHandler = expressionProcessors[expressionNode.t];
				if expressionHandler then
					expressionHandler(expressionNode);
					return;
				else
					print(expressionNode);
					error((("Unknown expr type %*"):format(expressionNode.t)));
					return;
				end;
			end;
		end;
		local function processCondition(conditionData)
			local conditionString = conditionData.condition;
			if conditionString ~= "exist" and conditionString ~= "not exist" then
				local rightHandSide = conditionData.rhs;
				if type(rightHandSide) == "number" then
					error("This should NOT have happened");
					return;
				elseif rightHandSide.is_full_condition_t then
					processCondition(rightHandSide);
					return;
				else
					processExpression(rightHandSide);
				end;
			end;
		end;
		local function replaceLineWithFunctionCall(lineData, functionData, arg2, arg3, v3221)
			local lines = lineData.lines;
			local lineIndex = table.find(lines, lineData);
			assert(lineIndex);
			assert(functionData.t == "function");
			if v3221 then
				assert(lineData.t == "set table");
			end;
			local newFunctionCallLine = createDefineFunctionMetadata(lines, functionData, arg2, arg3, v3221);
			newFunctionCallLine.reads = lineData.reads;
			newFunctionCallLine.writes = lineData.writes;
			for _, readDependency in ipairs(lineData.reads) do
				local reads = readDependency.reads;
				local readIndex = table.find(reads, lineData);
				assert(readIndex);
				reads[readIndex] = newFunctionCallLine;
			end;
			for _, writeDependency in ipairs(lineData.writes) do
				local writes = writeDependency.writes;
				local writeIndex = table.find(writes, lineData);
				assert(writeIndex);
				writes[writeIndex] = newFunctionCallLine;
			end;
			lines[lineIndex] = newFunctionCallLine;
			counter = counter + 1;
		end;
		local function emptyFunction(_)

		end;
		local function resolveOverrideExpressions(valueData)
			local value = valueData.value;
			while value.t == "name" do
				local overrideExpression = value.name.override_expr;
				if overrideExpression then
					value = overrideExpression;
				else
					break;
				end;
			end;
			local originalValue = value;
			value = valueData.lines;
			assert(value[lineIndex] == valueData);
			local nextLineIndex = lineIndex + 1;
			while true do
				local nextLine = value[nextLineIndex];
				if nextLine and nextLine.t == "set table" then
					local tableData = nextLine.table;
					while tableData.t == "name" do
						local overrideExpression = tableData.name.override_expr;
						if overrideExpression then
							tableData = overrideExpression;
						else
							break;
						end;
					end;
					local tableValue = tableData;
					if tableValue.t == "name" then
						tableData = tableValue.name;
						if originalValue == tableData.init_expr then
							local tableValue = nextLine.value;
							while tableValue.t == "name" do
								local overrideExpression = tableValue.name.override_expr;
								if overrideExpression then
									tableValue = overrideExpression;
								else
									break;
								end;
							end;
							local originalTableValue = tableValue;
							if originalTableValue.t == "name" then
								if not (originalTableValue.name.init_expr and originalTableValue.name ~= tableData) then
									break;
								end;
							elseif originalTableValue.contributors[originalValue] then
								break;
							end;
							local key = nextLine.key;
							while key.t == "name" do
								local overrideExpression = key.name.override_expr;
								if overrideExpression then
									key = overrideExpression;
								else
									break;
								end;
							end;
							tableValue = key;
							if not tableValue.contributors[originalValue] then
								if tableValue.t == "constant" then
									key = tableValue.const;
									if key.type == 2 then
										local keyValue = key.value;
										table.insert(originalValue.initializers_order, keyValue);
										originalValue.initializers[keyValue] = originalTableValue;
										table.remove(value, nextLineIndex);
										removeReadDependency(nextLine, tableData);
										counter = counter + 1;
										continue;
									end;
								end;
								table.insert(originalValue.initializers_order, tableValue);
								originalValue.initializers[tableValue] = originalTableValue;
								table.remove(value, nextLineIndex);
								removeReadDependency(nextLine, tableData);
								counter = counter + 1;
							else
								break;
							end;
						else
							break;
						end;
					else
						break;
					end;
				else
					break;
				end;
			end;
		end;
		local function _(v3252)
			if v3252.t == "if" and not v3252.else_ and #v3252.elseifs == 0 then
				local passStatement = v3252.pass;
				if #passStatement == 1 and passStatement[1].t == "break" then
					return true;
				end;
			end;
		end;
		local function hasTerminalStatement(statementList)
			if linesHadSkippedReturnMap[statementList] then
				return true;
			elseif #statementList == 0 then
				return false;
			else
				local lastElement = statementList[#statementList];
				if not (lastElement.t ~= "return" and lastElement.t ~= "break") or lastElement.t == "continue" then
					return true, lastElement;
				elseif lastElement.t == "do" then
					return hasTerminalStatement(lastElement.content);
				else
					return false;
				end;
			end;
		end;
		local function _(v3258)
			if not v3258 then
				return "empty return";
			else
				local statementType = v3258.t;
				if statementType == "return" then
					if #v3258.values == 0 then
						return "empty return";
					end;
				elseif statementType == "break" then
					return "break";
				elseif statementType == "continue" then
					return "continue";
				end;
				return;
			end;
		end;
		local processedVariables = {};
		local statementHandlers = {
			comment = emptyFunction, 
			["break"] = emptyFunction, 
			continue = emptyFunction, 
			["unknown jump"] = emptyFunction, 
			["define variable"] = function(valueData)
				local value = valueData.value;
				while value.t == "name" do
					local overrideExpression = value.name.override_expr;
					if overrideExpression then
						value = overrideExpression;
					else
						break;
					end;
				end;
				local originalValue = value;
				if originalValue.t == "new table" then
					resolveOverrideExpressions(valueData);
				end;
				processExpression(originalValue);
			end, 
			["set global"] = function(v3266)
				processExpression(v3266.value);
			end, 
			["define function"] = function(v3267)
				processExpression(v3267.func);
			end, 
			call = function(functionCallData)
				processExpression(functionCallData.func);
				for _, argumentValue in ipairs(functionCallData.args) do
					processExpression(argumentValue);
				end;
			end, 
			["set table"] = function(v3271)
				processExpression(v3271.table);
				processExpression(v3271.key);
				processExpression(v3271.value);
			end, 
			["set variable"] = function(v3272)
				processExpression(v3272.value);
			end, 
			["return"] = function(line)
				local lines = line.lines;
				local lineIndex = table.find(lines, line);
				assert(lineIndex);
				if lines[lineIndex + 1] or linesHadSkippedReturnMap[lines] then
					local linesTable = {
						line
					};
					lines[lineIndex] = {
						t = "do", 
						lines = lines, 
						reads = {}, 
						writes = {}, 
						content = linesTable
					};
					line.lines = linesTable;
					counter = counter + 1;
				end;
				for _, value in ipairs(line.values) do
					processExpression(value);
				end;
			end, 
			["if"] = function(conditionalBlock)
				processExpression(conditionalBlock.expr);
				processLine(conditionalBlock.pass);
				for _, elseifBlock in ipairs(conditionalBlock.elseifs) do
					processExpression(elseifBlock.expr);
					processLine(elseifBlock.code);
				end;
				local elseBlock = conditionalBlock.else_;
				if elseBlock then
					if #elseBlock == 1 then
						local elseIfBlock = elseBlock[1];
						while elseIfBlock.t == "if" do
							table.insert(conditionalBlock.elseifs, {
								expr = elseIfBlock.expr, 
								code = elseIfBlock.pass
							});
							counter = counter + 1;
							elseBlock = elseIfBlock.else_;
							conditionalBlock.else_ = elseBlock;
							if elseBlock and #elseBlock == 1 then
								elseIfBlock = elseBlock[1];
							else
								break;
							end;
						end;
						return;
					else
						processLine(elseBlock);
						return;
					end;
				else
					if settings.assume_if_else then
						local lines = conditionalBlock.lines;
						local lineIndex = table.find(lines, conditionalBlock);
						assert(lineIndex);
						if lineIndex < #lines then
							local passBlock = conditionalBlock.pass;
							local hasReturnOrBreak, returnOrBreakStatement;
							if linesHadSkippedReturnMap[passBlock] then
								hasReturnOrBreak = true;
								returnOrBreakStatement = nil;
							elseif #passBlock == 0 then
								hasReturnOrBreak = false;
								returnOrBreakStatement = nil;
							else
								local lastPassElement = passBlock[#passBlock];
								if not (lastPassElement.t ~= "return" and lastPassElement.t ~= "break") or lastPassElement.t == "continue" then
									hasReturnOrBreak = true;
									returnOrBreakStatement = lastPassElement;
								elseif lastPassElement.t == "do" then
									local passValue, passStatement = hasTerminalStatement(lastPassElement.content);
									hasReturnOrBreak = passValue;
									returnOrBreakStatement = passStatement;
								else
									hasReturnOrBreak = false;
									returnOrBreakStatement = nil;
								end;
							end;
							if hasReturnOrBreak then
								local lastStatement;
								if linesHadSkippedReturnMap[lines] then
									passBlock = true;
									lastStatement = nil;
								elseif #lines == 0 then
									passBlock = false;
									lastStatement = nil;
								else
									local lastLineElement = lines[#lines];
									if not (lastLineElement.t ~= "return" and lastLineElement.t ~= "break") or lastLineElement.t == "continue" then
										passBlock = true;
										lastStatement = lastLineElement;
									elseif lastLineElement.t == "do" then
										local passValue, passStatement = hasTerminalStatement(lastLineElement.content);
										passBlock = passValue;
										lastStatement = passStatement;
									else
										passBlock = false;
										lastStatement = nil;
									end;
								end;
								if passBlock then
									local controlFlowType;
									if not returnOrBreakStatement then
										controlFlowType = "empty return";
									else
										local lastPassElementType = returnOrBreakStatement.t;
										controlFlowType = if lastPassElementType == "return" then #returnOrBreakStatement.values == 0 and "empty return" or nil else lastPassElementType == "break" and "break" or lastPassElementType == "continue" and "continue" or nil;
									end;
									if controlFlowType then
										if #conditionalBlock.pass == 1 then
											return;
										else
											local controlFlowType;
											if not lastStatement then
												controlFlowType = "empty return";
											else
												local lastLineElementType = lastStatement.t;
												controlFlowType = if lastLineElementType == "return" then #lastStatement.values == 0 and "empty return" or nil else lastLineElementType == "break" and "break" or lastLineElementType == "continue" and "continue" or nil;
											end;
											if controlFlowType and controlFlowType == controlFlowType then
												local statementList = {};
												local nextLineIndex = lineIndex + 1;
												if returnOrBreakStatement then
													local removeLine = table.remove;
													local lines = returnOrBreakStatement.lines;
													local lineIndex = table.find(returnOrBreakStatement.lines, returnOrBreakStatement);
													assert(lineIndex);
													removeLine(lines, lineIndex);
												end;
												if lastStatement then
													local removeLine = table.remove;
													local lines = lastStatement.lines;
													local lineIndex = table.find(lastStatement.lines, lastStatement);
													assert(lineIndex);
													removeLine(lines, lineIndex);
												end;
												while lines[nextLineIndex] do
													local line = lines[nextLineIndex];
													assert(line);
													local line = line;
													table.remove(lines, nextLineIndex);
													if returnOrBreakStatement and line == returnOrBreakStatement then
														assert(not lines[nextLineIndex]);
														break;
													else
														line.lines = statementList;
														table.insert(statementList, line);
													end;
												end;
												conditionalBlock.else_ = statementList;
												if controlFlowType ~= "empty return" then
													if returnOrBreakStatement then
														table.insert(lines, returnOrBreakStatement);
														return;
													elseif lastStatement then
														table.insert(lines, lastStatement);
													end;
												end;
											end;
										end;
									end;
								end;
							end;
						end;
					end;
					return;
				end;
			end, 
			["while"] = function(whileLoop)
				local isFirstPass = false;
				local isSecondPass = false;
				if not whileLoop.expr then
					local hasBreak = nil;
					local expressions = {};
					local codeBlock = whileLoop.code;
					local expressionList = {};
					for codeIndex = #codeBlock, 1, -1 do
						local codeStatement = codeBlock[codeIndex];
						local shouldBreak;
						if codeStatement.t == "if" and not codeStatement.else_ and #codeStatement.elseifs == 0 then
							local passCode = codeStatement.pass;
							if #passCode == 1 and passCode[1].t == "break" then
								shouldBreak = true;
								isFirstPass = true;
							end;
						end;
						if not isFirstPass then
							shouldBreak = nil;
						end;
						isFirstPass = false;
						if shouldBreak then
							table.insert(expressionList, codeStatement.expr);
							codeBlock[codeIndex] = nil;
						else
							break;
						end;
					end;
					for expressionIndex = #expressionList, 1, -1 do
						table.insert(expressions, expressionList[expressionIndex]);
					end;
					if #expressions > 0 then
						hasBreak = true;
					else
						for _, codeStatement in ipairs(codeBlock) do
							local shouldBreakInner;
							if codeStatement.t == "if" and not codeStatement.else_ and #codeStatement.elseifs == 0 then
								local passBlock = codeStatement.pass;
								if #passBlock == 1 and passBlock[1].t == "break" then
									shouldBreakInner = true;
									isSecondPass = true;
								end;
							end;
							if not isSecondPass then
								shouldBreakInner = nil;
							end;
							isSecondPass = false;
							if shouldBreakInner then
								local conditionExpression = codeStatement.expr;
								local expression;
								if conditionExpression.t == "condition" then
									conditionExpression.condition = comparisonOperatorMap[conditionExpression.condition];
									expression = conditionExpression;
								else
									expression = if conditionExpression.t == "not" then conditionExpression.rhs else createNotMetadata(conditionExpression);
								end;
								table.insert(expressions, expression);
							else
								break;
							end;
						end;
						for _ = 1, #expressions do
							table.remove(codeBlock, 1);
						end;
					end;
					if #expressions > 0 then
						if #expressions == 1 then
							whileLoop.expr = expressions[1];
						else
							expressionList = expressions[1];
							for expressionIndex2 = 2, #expressions do
								expressionList = createAndMetadata(expressionList, expressions[expressionIndex2]);
							end;
							whileLoop.expr = expressionList;
						end;
						if hasBreak then
							whileLoop.t = "repeat";
						end;
					end;
				end;
				if whileLoop.expr then
					processExpression(whileLoop.expr);
				end;
				processLine(whileLoop.code);
			end, 
			["repeat"] = function(elseifData)
				if elseifData.expr then
					processExpression(elseifData.expr);
				end;
				processLine(elseifData.code);
			end, 
			["do"] = function(content)
				processLine(content.content);
			end, 
			["for"] = function(forLoopData)
				local forLoopInfo = forLoopData.for_info;
				local loopVariables = forLoopInfo.variables;
				assert(loopVariables);
				local loopType = forLoopInfo.type;
				if loopType == "numeric" then
					assert(#loopVariables == 1);
					if not processedVariables[forLoopData] then
						processedVariables[forLoopData] = true;
						local loopVariable = loopVariables[1];
						local loopIndexName = variableIndexMap.i;
						local uniqueIndexName = "i";
						local indexCounter = loopIndexName or 1;
						while variableMap[uniqueIndexName] or globalNameCache[uniqueIndexName] do
							indexCounter = indexCounter + 1;
							uniqueIndexName = "i" .. "_" .. indexCounter;
						end;
						variableIndexMap.i = indexCounter;
						local renamedIndexName = uniqueIndexName;
						loopIndexName = loopVariable.name;
						if not variableMap[loopIndexName] then
							error((("[write] Variable %* not allocated"):format(loopIndexName)));
						end;
						variableMap[loopIndexName] = nil;
						loopVariable.name = renamedIndexName;
						variableMap[renamedIndexName] = loopVariable;
						loopVariable.attributes.renamed = true;
					end;
					local numericForLoopArgs = forLoopInfo.args;
					local indexExpression = numericForLoopArgs.index_expr;
					assert(indexExpression);
					processExpression(indexExpression);
					local endExpression = numericForLoopArgs.end_expr;
					assert(endExpression);
					processExpression(endExpression);
					local stepExpression = numericForLoopArgs.step_expr;
					assert(stepExpression);
					processExpression(stepExpression);
				elseif loopType == "generic" then
					assert(#loopVariables > 0);
					if not processedVariables[forLoopData] then
						processedVariables[forLoopData] = true;
						local loopVariable = loopVariables[1];
						local loopIndexName = variableIndexMap.i;
						local uniqueIndexName = "i";
						local indexCounter = loopIndexName or 1;
						while variableMap[uniqueIndexName] or globalNameCache[uniqueIndexName] do
							indexCounter = indexCounter + 1;
							uniqueIndexName = "i" .. "_" .. indexCounter;
						end;
						variableIndexMap.i = indexCounter;
						local renamedIndexName = uniqueIndexName;
						loopIndexName = loopVariable.name;
						if not variableMap[loopIndexName] then
							error((("[write] Variable %* not allocated"):format(loopIndexName)));
						end;
						variableMap[loopIndexName] = nil;
						loopVariable.name = renamedIndexName;
						variableMap[renamedIndexName] = loopVariable;
						loopVariable.attributes.renamed = true;
						if #loopVariables == 2 then
							loopVariable = loopVariables[2];
							loopIndexName = variableIndexMap.v;
							uniqueIndexName = "v";
							indexCounter = loopIndexName or 1;
							while variableMap[uniqueIndexName] or globalNameCache[uniqueIndexName] do
								indexCounter = indexCounter + 1;
								uniqueIndexName = "v" .. "_" .. indexCounter;
							end;
							variableIndexMap.v = indexCounter;
							renamedIndexName = uniqueIndexName;
							loopIndexName = loopVariable.name;
							if not variableMap[loopIndexName] then
								error((("[write] Variable %* not allocated"):format(loopIndexName)));
							end;
							variableMap[loopIndexName] = nil;
							loopVariable.name = renamedIndexName;
							variableMap[renamedIndexName] = loopVariable;
							loopVariable.attributes.renamed = true;
						else
							for variableIndex = 2, #loopVariables do
								uniqueIndexName = loopVariables[variableIndex];
								local variableName = "v" .. variableIndex - 1;
								local variableCounter = variableIndexMap[variableName];
								local renamedVariableName = variableName;
								local variableCounter = variableCounter or 1;
								while variableMap[renamedVariableName] or globalNameCache[renamedVariableName] do
									variableCounter = variableCounter + 1;
									renamedVariableName = variableName .. "_" .. variableCounter;
								end;
								variableIndexMap[variableName] = variableCounter;
								indexCounter = renamedVariableName;
								variableName = uniqueIndexName.name;
								if not variableMap[variableName] then
									error((("[write] Variable %* not allocated"):format(variableName)));
								end;
								variableMap[variableName] = nil;
								uniqueIndexName.name = indexCounter;
								variableMap[indexCounter] = uniqueIndexName;
								uniqueIndexName.attributes.renamed = true;
							end;
						end;
					end;
					local genericForLoopArgs = forLoopInfo.args;
					local generatorExpression = genericForLoopArgs.generator_expr;
					assert(generatorExpression);
					processExpression(generatorExpression);
					if genericForLoopArgs.state_expr then
						processExpression(genericForLoopArgs.state_expr);
					end;
					if genericForLoopArgs.index_expr then
						processExpression(genericForLoopArgs.index_expr);
					end;
				else
					error((("Unknown for_type \"%*\""):format(loopType)));
				end;
				processLine(forLoopData.code);
			end
		};
		local variableDefinitions = {
			{}, 
			{
				["define variable"] = function(variableData)
					local variableValue = variableData.value;
					while variableValue.t == "name" do
						local overrideExpression8 = variableValue.name.override_expr;
						if overrideExpression8 then
							variableValue = overrideExpression8;
						else
							break;
						end;
					end;
					local localValueCopy = variableValue;
					if localValueCopy.t == "new table" then
						resolveOverrideExpressions(variableData);
					elseif localValueCopy.t == "function" then
						assert(#variableData.names == 1);
						variableValue = variableData.names[1];
						assert(variableValue == localValueCopy.varname);
						local newName = localValueCopy.name;
						local variableName = variableValue.name;
						if not variableMap[variableName] then
							error((("[write] Variable %* not allocated"):format(variableName)));
						end;
						variableMap[variableName] = nil;
						variableValue.name = newName;
						variableMap[newName] = variableValue;
						variableValue.attributes.renamed = true;
						replaceLineWithFunctionCall(variableData, localValueCopy, variableValue, "local");
					end;
					processExpression(localValueCopy);
				end, 
				["set variable"] = function(variable7)
					local value7 = variable7.value;
					while value7.t == "name" do
						local overrideExpression9 = value7.name.override_expr;
						if overrideExpression9 then
							value7 = overrideExpression9;
						else
							break;
						end;
					end;
					local localValue7Copy = value7;
					if localValue7Copy.t == "function" then
						value7 = variable7.name;
						local newName11 = localValue7Copy.name;
						if settings.smart_var_level >= 2 then
							local name12 = value7.name;
							if not variableMap[name12] then
								error((("[write] Variable %* not allocated"):format(name12)));
							end;
							variableMap[name12] = nil;
							value7.name = newName11;
							variableMap[newName11] = value7;
							value7.attributes.renamed = true;
						end;
						replaceLineWithFunctionCall(variable7, localValue7Copy, value7, "global");
					end;
					processExpression(variable7.value);
				end, 
				["set global"] = function(variable8)
					local value8 = variable8.value;
					while value8.t == "name" do
						local overrideExpression10 = value8.name.override_expr;
						if overrideExpression10 then
							value8 = overrideExpression10;
						else
							break;
						end;
					end;
					local localValue8Copy = value8;
					if localValue8Copy.t == "function" then
						value8 = variable8.name;
						if value8.type == 3 then
							local v3233Function = replaceLineWithFunctionCall;
							local variable8Copy = variable8;
							local localLocalValue8Copy = localValue8Copy;
							local value9 = value8.value;
							v3233Function(variable8Copy, localLocalValue8Copy, variableMap[value9] or allocateVariable(value9, {
								beginning = -1, 
								ending = -1
							}, {}), "global");
						end;
					end;
					processExpression(localValue8Copy);
				end, 
				["set table"] = function(variable10)
					local value10 = variable10.value;
					while value10.t == "name" do
						local overrideExpression11 = value10.name.override_expr;
						if overrideExpression11 then
							value10 = overrideExpression11;
						else
							break;
						end;
					end;
					local localValue10Copy = value10;
					if localValue10Copy.t == "function" then
						local table1 = variable10.table;
						while table1.t == "name" do
							local overrideExpression12 = table1.name.override_expr;
							if overrideExpression12 then
								table1 = overrideExpression12;
							else
								break;
							end;
						end;
						value10 = table1;
						table1 = value10.t;
						local value3384 = nil;
						if table1 == "global" then
							local name13 = value10.name;
							if name13.type == 3 then
								value3384 = name13.value;
							end;
						elseif table1 == "name" then
							value3384 = value10.name.name;
						end;
						if value3384 and isValidIdentifier(value3384) then
							local value3386 = nil;
							local keyData = variable10.key;
							if keyData.t == "constant" then
								local constantData = keyData.const;
								if constantData.type == 3 then
									local value11 = constantData.value;
									if isValidIdentifier(value11) then
										assert(type(value11) == "string");
										value3386 = value11;
									end;
								end;
							end;
							if value3386 then
								local v3233Function1 = replaceLineWithFunctionCall;
								local variable10Copy = variable10;
								local localLocalValue10Copy = localValue10Copy;
								local value3386Copy = value3386;
								local cachedValue = variableMap[value3386Copy] or allocateVariable(value3386Copy, {
									beginning = -1, 
									ending = -1
								}, {});
								value3386Copy = "table";
								local dataList = {};
								local value3384Copy = value3384;
								local value3397 = variableMap[value3384Copy] or allocateVariable(value3384Copy, {
									beginning = -1, 
									ending = -1
								}, {});
								local value3386Copy1 = value3386;
								value3384Copy = variableMap[value3386Copy1] or allocateVariable(value3386Copy1, {
									beginning = -1, 
									ending = -1
								}, {});
								arrayCopyFunction(dataList, 1, value3397, value3384Copy);
								v3233Function1(variable10Copy, localLocalValue10Copy, cachedValue, value3386Copy, dataList);
							end;
						end;
						processExpression(value10);
						processExpression(variable10.key);
						processExpression(localValue10Copy);
						return;
					else
						processExpression(variable10.table);
						processExpression(variable10.key);
						processExpression(localValue10Copy);
						return;
					end;
				end
			}
		};
		assert(#variableDefinitions > 0);
		local clonedTable = nil;
		processLine = function(lineTypes)
			local originalV1203 = linesList;
			local v3181_local = lineIndex;
			linesList = lineTypes;
			lineIndex = 1;
			while true do
				if lineIndex <= #lineTypes then
					local lineData = lineTypes[lineIndex];
					if processedNodes[lineData] then
						return;
					else
						processedNodes[lineData] = true;
						local lineTypeHandler = clonedTable[lineData.t];
						if lineTypeHandler then
							lineTypeHandler(lineData);
							lineIndex = lineIndex + 1;
						else
							print(lineData);
							error((("Unknown line type %*"):format(lineData.t)));
						end;
					end;
				else
					linesList = originalV1203;
					lineIndex = v3181_local;
					return;
				end;
			end;
		end;
		local function _()
			counter = 0;
		end;
		local function processExpressions()
			counter = 0;
			local processedExpressions = {};
			for _, expressionData in pairs(table.clone(variableMap)) do
				if expressionData.init_expr then
					assert(expressionData.init_expr);
					local expressionAttributes = expressionData.attributes;
					if not expressionAttributes.no_inline and not expressionAttributes.is_upvalue then
						local readCount = #expressionData.reads;
						if readCount == 1 then
							if #expressionData.writes == 1 then
								if expressionAttributes.multireg then
									local readExpression = expressionData.reads[1];
									if not processedExpressions[readExpression] and readExpression.t == "for" then
										processedExpressions[readExpression] = true;
										local variableList = expressionData.var_list;
										assert(#variableList > 1);
										if #variableList < 4 then
											local writeExpression = expressionData.writes[1];
											local initializationExpression = expressionData.init_expr;
											assert(initializationExpression);
											local canInline = true;
											for _, variableData in ipairs(variableList) do
												if variableData ~= expressionData then
													if not (#variableData.reads == 1 and #variableData.writes == 1) or variableData.reads[1] ~= readExpression then
														canInline = false;
														break;
													else
														assert(variableData.writes[1] == writeExpression);
														assert(variableData.init_expr == initializationExpression);
													end;
												end;
											end;
											if canInline then
												local forLoopInfo = readExpression.for_info;
												assert(forLoopInfo.type == "generic");
												local forLoopArguments = forLoopInfo.args;
												local generatorExpression = forLoopArguments.generator_expr;
												local forLoopStateExpression = forLoopArguments.state_expr;
												local forLoopIndexExpression = forLoopArguments.index_expr;
												for index, variable in ipairs(variableList) do
													assert(variable.var_num == index);
												end;
												canInline = false;
												if #variableList == 2 then
													if forLoopStateExpression then
														local nestedStateExpression = forLoopStateExpression;
														local variable2 = variableList[1];
														local deepNestedStateExpression = nestedStateExpression;
														while deepNestedStateExpression.t == "name" do
															local overrideExpression13 = deepNestedStateExpression.name.override_expr;
															if overrideExpression13 then
																deepNestedStateExpression = overrideExpression13;
															else
																break;
															end;
														end;
														nestedStateExpression = deepNestedStateExpression;
														if nestedStateExpression.t == "name" and nestedStateExpression.name == variable2 and forLoopIndexExpression then
															nestedStateExpression = forLoopIndexExpression;
															variable2 = variableList[2];
															deepNestedStateExpression = nestedStateExpression;
															while deepNestedStateExpression.t == "name" do
																local overrideExpression14 = deepNestedStateExpression.name.override_expr;
																if overrideExpression14 then
																	deepNestedStateExpression = overrideExpression14;
																else
																	break;
																end;
															end;
															nestedStateExpression = deepNestedStateExpression;
															if nestedStateExpression.t == "name" and nestedStateExpression.name == variable2 then
																canInline = true;
																forLoopArguments.state_expr = initializationExpression;
																forLoopArguments.index_expr = nil;
															end;
														end;
													end;
												elseif generatorExpression then
													local nestedGeneratorExpression = generatorExpression;
													local variable3 = variableList[1];
													local deepNestedGeneratorExpression = nestedGeneratorExpression;
													while deepNestedGeneratorExpression.t == "name" do
														local overrideExpression15 = deepNestedGeneratorExpression.name.override_expr;
														if overrideExpression15 then
															deepNestedGeneratorExpression = overrideExpression15;
														else
															break;
														end;
													end;
													nestedGeneratorExpression = deepNestedGeneratorExpression;
													if nestedGeneratorExpression.t == "name" and nestedGeneratorExpression.name == variable3 and forLoopStateExpression then
														nestedGeneratorExpression = forLoopStateExpression;
														variable3 = variableList[2];
														deepNestedGeneratorExpression = nestedGeneratorExpression;
														while deepNestedGeneratorExpression.t == "name" do
															local overrideExpression16 = deepNestedGeneratorExpression.name.override_expr;
															if overrideExpression16 then
																deepNestedGeneratorExpression = overrideExpression16;
															else
																break;
															end;
														end;
														nestedGeneratorExpression = deepNestedGeneratorExpression;
														if nestedGeneratorExpression.t == "name" and nestedGeneratorExpression.name == variable3 and forLoopIndexExpression then
															nestedGeneratorExpression = forLoopIndexExpression;
															variable3 = variableList[3];
															deepNestedGeneratorExpression = nestedGeneratorExpression;
															while deepNestedGeneratorExpression.t == "name" do
																local overrideExpression17 = deepNestedGeneratorExpression.name.override_expr;
																if overrideExpression17 then
																	deepNestedGeneratorExpression = overrideExpression17;
																else
																	break;
																end;
															end;
															nestedGeneratorExpression = deepNestedGeneratorExpression;
															if nestedGeneratorExpression.t == "name" and nestedGeneratorExpression.name == variable3 then
																canInline = true;
																forLoopArguments.generator_expr = initializationExpression;
																forLoopArguments.state_expr = nil;
																forLoopArguments.index_expr = nil;
															end;
														end;
													end;
												end;
												if canInline then
													local writeExpressionLines = writeExpression.lines;
													local lineIndex = table.find(writeExpressionLines, writeExpression);
													assert(lineIndex);
													removeReadDependency(readExpression, expressionData);
													removeWriteDependency(writeExpression, expressionData);
													table.remove(writeExpressionLines, lineIndex);
													assert(expressionData.init_expr);
													counter = counter + 1;
												end;
											end;
										end;
									end;
								else
									local readExpression = expressionData.reads[1];
									local writeExpression = expressionData.writes[1];
									if not reservedWords[writeExpression.t] then
										local writeExpressionLines = writeExpression.lines;
										local lineIndex2 = table.find(writeExpressionLines, writeExpression);
										assert(lineIndex2);
										removeReadDependency(readExpression, expressionData);
										removeWriteDependency(writeExpression, expressionData);
										table.remove(writeExpressionLines, lineIndex2);
										local initialExpression = expressionData.init_expr;
										local variableName = expressionData.name;
										if not variableMap[variableName] then
											error((("[write] Variable %* not allocated"):format(variableName)));
										end;
										variableMap[variableName] = nil;
										expressionData.override_expr = initialExpression;
										counter = counter + 1;
									end;
								end;
							end;
						elseif readCount == 0 and not expressionAttributes._ then
							local variableName2 = expressionData.name;
							if not variableMap[variableName2] then
								error((("[write] Variable %* not allocated"):format(variableName2)));
							end;
							variableMap[variableName2] = nil;
							expressionData.name = "_";
							variableMap._ = expressionData;
							expressionData.attributes.renamed = true;
							expressionAttributes._ = true;
							counter = counter + 1;
						end;
					end;
				end;
			end;
		end;
		local function processAllExpressions()
			counter = 0;
			clonedTable = table.clone(statementHandlers);
			for _, expressionSet in ipairs(variableDefinitions) do
				for expressionType, expressionHandler in pairs(expressionSet) do
					clonedTable[expressionType] = expressionHandler;
				end;
				while true do
					processedNodes = {};
					processLine(linesList);
					if counter ~= 0 then
						counter = 0;
					else
						break;
					end;
				end;
				counter = 0;
			end;
		end;
		local function _()
			local counter1 = -1;
			while true do
				counter1 = counter1 + 1;
				processExpressions();
				if not (counter ~= 0) then
					break;
				end;
			end;
			return counter1;
		end;
		local function _()
			local counter2 = -1;
			while true do
				counter2 = counter2 + 1;
				processAllExpressions();
				if not (counter ~= 0) then
					break;
				end;
			end;
			return counter2;
		end;
		local sumValue = nil;
		while true do
			sumValue = 0;
			local expressionIndex = -1;
			while true do
				expressionIndex = expressionIndex + 1;
				processAllExpressions();
				if not (counter ~= 0) then
					break;
				end;
			end;
			sumValue = sumValue + expressionIndex;
			expressionIndex = -1;
			while true do
				expressionIndex = expressionIndex + 1;
				processExpressions();
				if not (counter ~= 0) then
					break;
				end;
			end;
			if not (sumValue + expressionIndex ~= 0) then
				break;
			end;
		end;
		benchmarkTimer:end_benchmark("Global High Level Reductions");
		benchmarkTimer:start_benchmark("Smart Naming");
		local disableSmartVariables = settings.smart_var_level >= 3 and "NONE" or nil;
		local useExtensivePrefixes = settings.smart_var_extensive_prefixes and true or nil;
		local function _(returnValue)
			if useExtensivePrefixes then
				return returnValue;
			else
				return "";
			end;
		end;
		local function _(expression)
			local readExpressions = expression.reads;
			local lastRead = readExpressions[#readExpressions];
			if lastRead and lastRead.t == "return" then
				local values = {};
				local values = lastRead.values;
				if settings.smart_var_level == 3 then
					return true;
				elseif #values == 1 then
					values = values;
					return;
				else
					return;
				end;
			else
				return;
			end;
		end;
		local function getTableString(tableInfo)
			local isGetTable = true;
			if tableInfo.t ~= "constant index" then
				isGetTable = tableInfo.t == "get table";
			end;
			assert(isGetTable);
			local tableString = "";
			isGetTable = nil;
			local tableType, tableIndex;
			while true do
				if tableInfo.t == "constant index" then
					tableString = "." .. tableInfo.index.value .. tableString;
				elseif tableInfo.t == "get table" then
					tableIndex = tableInfo.index;
					if tableIndex.t == "constant" then
						tableString = "." .. constantToString(tableIndex.const, settings.string_quotes_behavior, true) .. tableString;
					else
						tableString = ".any" .. tableString;
					end;
				else
					assert(nil);
				end;
				tableIndex = tableInfo.table;
				tableType = tableIndex.t;
				if not (tableType ~= "constant index") or tableType.t == "get table" then
					tableInfo = tableIndex;
				else
					break;
				end;
			end;
			if tableType == "global" then
				return constantToString(tableIndex.name, settings.string_quotes_behavior, true) .. tableString, isGetTable;
			else
				return "any" .. tableString, true;
			end;
		end;
		local _ = function(originalString)
			local _ = nil;
			local reversedString = string.reverse(originalString);
			local dotIndex = string.find(reversedString, "%.");
			if dotIndex then
				return (string.sub(originalString, #originalString - dotIndex + 2, #originalString));
			else
				return originalString;
			end;
		end;
		local methodMap = {
			GetMouse = function(variableData1, _)
				if settings.smart_var_level >= 3 then
					local varNum = variableData1.var_num;
					assert(varNum);
					if varNum > 1 then
						return disableSmartVariables;
					end;
				end;
				return "mouse";
			end, 
			Clone = function(variableData2, _)
				if settings.smart_var_level >= 3 then
					local varNum = variableData2.var_num;
					assert(varNum);
					if varNum > 1 then
						return disableSmartVariables;
					end;
				end;
				return "clone";
			end, 
			GetChildren = function(variableData3, _)
				if settings.smart_var_level >= 3 then
					local varNum = variableData3.var_num;
					assert(varNum);
					if varNum > 1 then
						return disableSmartVariables;
					end;
				end;
				return "children";
			end, 
			GetDescendants = function(variableData4, _)
				if settings.smart_var_level >= 3 then
					local varNum = variableData4.var_num;
					assert(varNum);
					if varNum > 1 then
						return disableSmartVariables;
					end;
				end;
				return "descendants";
			end, 
			GetPlayers = function(variableData5, _)
				if settings.smart_var_level >= 3 then
					local varNum = variableData5.var_num;
					assert(varNum);
					if varNum > 1 then
						return disableSmartVariables;
					end;
				end;
				return "players";
			end, 
			format = function(variableData6, functionCall)
				if settings.smart_var_level >= 3 then
					local varNum = variableData6.var_num;
					assert(varNum);
					if varNum > 1 then
						return disableSmartVariables;
					end;
				end;
				if useExtensivePrefixes then
					local argumentCount = #functionCall.args;
					if argumentCount == 1 then
						return "formatted_1_value";
					else
						return "formatted_" .. argumentCount .. "_values";
					end;
				else
					return "formatted";
				end;
			end, 
			IsA = function(variableData7, _)
				if settings.smart_var_level >= 3 then
					local varNum = variableData7.var_num;
					assert(varNum);
					if varNum > 1 then
						return disableSmartVariables;
					end;
				end;
				return "children";
			end, 
			WaitForChild = function(variableData8, argumentData1)
				if settings.smart_var_level >= 3 then
					local varNum = variableData8.var_num;
					assert(varNum);
					if varNum > 1 then
						return disableSmartVariables;
					end;
				end;
				local args = argumentData1.args;
				if settings.smart_var_level >= 3 and #args == 0 then
					return (useExtensivePrefixes and "instance_" or "") .. "NEVER";
				else
					local neverInstance = nil;
					local arg1 = args[1];
					if arg1.t == "constant" and settings.smart_var_level >= 2 then
						local const = arg1.const;
						if const.type == 3 then
							return (useExtensivePrefixes and "instance_" or "") .. sanitizeString(const.value, true, true);
						elseif settings.smart_var_level >= 3 then
							neverInstance = true;
						end;
					end;
					if neverInstance then
						return (useExtensivePrefixes and "instance_" or "") .. "NEVER";
					elseif settings.smart_var_level >= 3 then
						return (useExtensivePrefixes and "instance_" or "") .. "SOME";
					elseif settings.smart_var_level >= 2 then
						return "instance";
					else
						return;
					end;
				end;
			end, 
			FindFirstChildOfClass = function(variableData9, argumentData2)
				if settings.smart_var_level >= 3 then
					local varNum = variableData9.var_num;
					assert(varNum);
					if varNum > 1 then
						return disableSmartVariables;
					end;
				end;
				local args = argumentData2.args;
				if settings.smart_var_level >= 3 and #args == 0 then
					return (useExtensivePrefixes and "instance_" or "") .. "NEVER";
				else
					local neverInstance2 = nil;
					local arg1 = args[1];
					if arg1.t == "constant" and settings.smart_var_level >= 2 then
						local const = arg1.const;
						if const.type == 3 then
							return (useExtensivePrefixes and "instance_with_" or "") .. "class_" .. sanitizeString(const.value, true, true);
						elseif settings.smart_var_level >= 3 then
							neverInstance2 = true;
						end;
					end;
					if neverInstance2 then
						return (useExtensivePrefixes and "instance_" or "") .. "NEVER";
					elseif settings.smart_var_level >= 2 then
						return "instance";
					else
						return;
					end;
				end;
			end, 
			FindFirstChildWhichIsA = function(variableData10, argumentData3)
				if settings.smart_var_level >= 3 then
					local varNum = variableData10.var_num;
					assert(varNum);
					if varNum > 1 then
						return disableSmartVariables;
					end;
				end;
				local args = argumentData3.args;
				if settings.smart_var_level >= 3 and #args == 0 then
					return (useExtensivePrefixes and "instance_" or "") .. "NEVER";
				else
					local neverInstance3 = nil;
					local arg1 = args[1];
					if arg1.t == "constant" and settings.smart_var_level >= 2 then
						local const = arg1.const;
						if const.type == 3 then
							return (useExtensivePrefixes and "instance_which_is_a_" or "") .. "class_" .. sanitizeString(const.value, true, true);
						elseif settings.smart_var_level >= 3 then
							neverInstance3 = true;
						end;
					end;
					if neverInstance3 then
						return (useExtensivePrefixes and "instance_" or "") .. "NEVER";
					elseif settings.smart_var_level >= 2 then
						return "instance";
					else
						return;
					end;
				end;
			end, 
			GetAttribute = function(variableData11, argumentData4)
				if settings.smart_var_level >= 3 then
					local varNum = variableData11.var_num;
					assert(varNum);
					if varNum > 1 then
						return disableSmartVariables;
					end;
				end;
				local args = argumentData4.args;
				if settings.smart_var_level >= 3 and #args == 0 then
					return "NEVER";
				else
					local neverAttribute = nil;
					local arg1 = args[1];
					if arg1.t == "constant" and settings.smart_var_level >= 2 then
						local constantValue = arg1.const;
						if constantValue.type == 3 then
							return (useExtensivePrefixes and "attribute_" or "") .. sanitizeString(constantValue.value, true, true);
						elseif settings.smart_var_level >= 3 then
							neverAttribute = true;
						end;
					end;
					if neverAttribute then
						return "NEVER";
					elseif settings.smart_var_level >= 2 then
						return "attribute";
					else
						return;
					end;
				end;
			end, 
			GetAttributeChangedSignal = function(variableData12, argumentData5)
				if settings.smart_var_level >= 3 then
					local variableNumber12 = variableData12.var_num;
					assert(variableNumber12);
					if variableNumber12 > 1 then
						return disableSmartVariables;
					end;
				end;
				local arguments10 = argumentData5.args;
				if settings.smart_var_level >= 3 and #arguments10 == 0 then
					return "NEVER";
				else
					local neverAttributeChangedSignal = nil;
					local argument1 = arguments10[1];
					if argument1.t == "constant" and settings.smart_var_level >= 2 then
						local constantValue6 = argument1.const;
						if constantValue6.type == 3 then
							return (useExtensivePrefixes and "attribute_" or "") .. sanitizeString(constantValue6.value, true, true) .. "_changed_signal";
						elseif settings.smart_var_level >= 3 then
							neverAttributeChangedSignal = true;
						end;
					end;
					if neverAttributeChangedSignal then
						return "NEVER";
					elseif settings.smart_var_level >= 2 then
						return "attribute_changed_signal";
					else
						return;
					end;
				end;
			end, 
			GetPropertyChangedSignal = function(variableData13, argumentData6)
				if settings.smart_var_level >= 3 then
					local variableNumber13 = variableData13.var_num;
					assert(variableNumber13);
					if variableNumber13 > 1 then
						return disableSmartVariables;
					end;
				end;
				local arguments11 = argumentData6.args;
				if settings.smart_var_level >= 3 and #arguments11 == 0 then
					return "NEVER";
				else
					local neverPropertyChangedSignal = nil;
					local argument11 = arguments11[1];
					if argument11.t == "constant" and settings.smart_var_level >= 2 then
						local constantValue7 = argument11.const;
						if constantValue7.type == 3 then
							return (useExtensivePrefixes and "property_" or "") .. sanitizeString(constantValue7.value, true, true) .. "_changed_signal";
						elseif settings.smart_var_level >= 3 then
							neverPropertyChangedSignal = true;
						end;
					end;
					if neverPropertyChangedSignal then
						return "NEVER";
					elseif settings.smart_var_level >= 2 then
						return "property_changed_signal";
					else
						return;
					end;
				end;
			end
		};
		local function _(key2, key)
			local valueAtIndex = methodMap[key];
			assert(valueAtIndex);
			methodMap[key2] = valueAtIndex;
		end;
		local cloneFunction = methodMap.Clone;
		assert(cloneFunction);
		methodMap.clone = cloneFunction;
		cloneFunction = methodMap.GetChildren;
		assert(cloneFunction);
		methodMap.getChildren = cloneFunction;
		cloneFunction = methodMap.GetChildren;
		assert(cloneFunction);
		methodMap.children = cloneFunction;
		cloneFunction = methodMap.WaitForChild;
		assert(cloneFunction);
		methodMap.FindFirstChild = cloneFunction;
		cloneFunction = methodMap.FindFirstChild;
		assert(cloneFunction);
		methodMap.findFirstChild = cloneFunction;
		cloneFunction = methodMap.FindFirstChild;
		assert(cloneFunction);
		methodMap.FindFirstAncestor = cloneFunction;
		cloneFunction = methodMap.FindFirstChildOfClass;
		assert(cloneFunction);
		methodMap.FindFirstAncestorOfClass = cloneFunction;
		cloneFunction = methodMap.FindFirstChildWhichIsA;
		assert(cloneFunction);
		methodMap.FindFirstAncestorWhichIsA = cloneFunction;
		cloneFunction = methodMap.FindFirstChild;
		assert(cloneFunction);
		methodMap.FindFirstDescendant = cloneFunction;
		cloneFunction = {
			["Instance.new"] = function(variableData14, argumentData7)
				local isParented = variableData14.var_num;
				assert(isParented);
				if isParented > 1 then
					return disableSmartVariables;
				else
					local arguments12 = argumentData7.args;
					if #arguments12 == 0 then
						return (useExtensivePrefixes and "instance_" or "") .. "NEVER";
					else
						isParented = nil;
						if #arguments12 > 1 and settings.smart_var_level >= 2 and arguments12[2].t ~= "nil" then
							isParented = true;
						end;
						local formattedValue = nil;
						local argument12 = arguments12[1];
						if argument12.t == "constant" then
							local unquotedValue = constantToString(argument12.const, settings.string_quotes_behavior, true);
							formattedValue = sanitizeString(unquotedValue, true, true);
						else
							formattedValue = "any";
						end;
						if isParented then
							return (useExtensivePrefixes and "instance_" or "") .. formattedValue .. (useExtensivePrefixes and "_parented" or "");
						else
							return (useExtensivePrefixes and "instance_" or "") .. formattedValue;
						end;
					end;
				end;
			end, 
			["math.random"] = function(variableNumber1, argumentData8)
				local variableNumber15 = variableNumber1.var_num;
				assert(variableNumber15);
				if variableNumber15 > 1 then
					return disableSmartVariables;
				else
					local argumentCount = #argumentData8.args;
					if argumentCount == 0 and settings.smart_var_level >= 2 then
						return "seed";
					elseif settings.smart_var_level >= 3 then
						if argumentCount == 1 then
							return "randint_from_1";
						else
							return "randint";
						end;
					else
						return "randint";
					end;
				end;
			end, 
			["math.sqrt"] = function(variableNumber2, requiresSquareRoot)
				local variableNumber16 = variableNumber2.var_num;
				assert(variableNumber16);
				if variableNumber16 > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresSquareRoot.args == 0 then
						return "NEVER";
					else
						return "squareroot";
					end;
				elseif settings.smart_var_level >= 2 then
					return "root";
				else
					return "num";
				end;
			end, 
			["math.abs"] = function(variableNumber3, requiresAbsoluteValue)
				local variableNumber17 = variableNumber3.var_num;
				assert(variableNumber17);
				if variableNumber17 > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresAbsoluteValue.args == 0 then
						return "NEVER";
					else
						return "absolute";
					end;
				elseif settings.smart_var_level >= 2 then
					return "absolute";
				else
					return "num";
				end;
			end, 
			["math.floor"] = function(variableNumber4, argumentData9)
				local additionOperands = variableNumber4.var_num;
				assert(additionOperands);
				if additionOperands > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					local operationArgument = argumentData9.args[1];
					if operationArgument then
						if operationArgument.t == "addition" then
							additionOperands = {
								operationArgument.lhs, 
								operationArgument.rhs
							};
							local stateCounter = 0;
							for _, operand in ipairs(additionOperands) do
								if operand.t == "constant" then
									local operandConstant = operand.const;
									if operandConstant.type == 2 and operandConstant.value == 0.5 then
										stateCounter = stateCounter + 1;
									end;
								end;
							end;
							if stateCounter == 2 then
								return "one";
							elseif stateCounter == 1 then
								return "rounded";
							else
								return "floored";
							end;
						else
							return "floored";
						end;
					else
						return "NEVER";
					end;
				elseif settings.smart_var_level >= 2 then
					return "floored";
				else
					return "num";
				end;
			end, 
			["math.ceil"] = function(variableNumber5, requiresCeiling)
				local variableNumber19 = variableNumber5.var_num;
				assert(variableNumber19);
				if variableNumber19 > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresCeiling.args == 0 then
						return "NEVER";
					else
						return "ceiled";
					end;
				elseif settings.smart_var_level >= 2 then
					return "ceiled";
				else
					return "num";
				end;
			end, 
			["math.round"] = function(variableNumber6, requiresRounding)
				local variableNumber20 = variableNumber6.var_num;
				assert(variableNumber20);
				if variableNumber20 > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresRounding.args == 0 then
						return "NEVER";
					else
						return "rounded";
					end;
				elseif settings.smart_var_level >= 2 then
					return "rounded";
				else
					return "num";
				end;
			end, 
			["math.sin"] = function(variableNumber7, requiresSine)
				local variableNumber21 = variableNumber7.var_num;
				assert(variableNumber21);
				if variableNumber21 > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresSine.args == 0 then
						return "NEVER";
					else
						return "sine";
					end;
				elseif settings.smart_var_level >= 2 then
					return "sine";
				else
					return "num";
				end;
			end, 
			["math.sinh"] = function(variableNumber8, requiresHyperbolicSine)
				local variableNumber22 = variableNumber8.var_num;
				assert(variableNumber22);
				if variableNumber22 > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresHyperbolicSine.args == 0 then
						return "NEVER";
					else
						return "hypsine";
					end;
				elseif settings.smart_var_level >= 2 then
					return "hypsine";
				else
					return "num";
				end;
			end, 
			["math.asin"] = function(variableNumber9, requiresArcSine)
				local variableNumber23 = variableNumber9.var_num;
				assert(variableNumber23);
				if variableNumber23 > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresArcSine.args == 0 then
						return "NEVER";
					else
						return "arcsine";
					end;
				elseif settings.smart_var_level >= 2 then
					return "arcsine";
				else
					return "num";
				end;
			end, 
			["math.cos"] = function(variableNumber10, requiresCosine)
				local variableNumber24 = variableNumber10.var_num;
				assert(variableNumber24);
				if variableNumber24 > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresCosine.args == 0 then
						return "NEVER";
					else
						return "cosine";
					end;
				elseif settings.smart_var_level >= 2 then
					return "cosine";
				else
					return "num";
				end;
			end, 
			["math.cosh"] = function(variableNumber11, requiresHyperbolicCosine)
				local variableNumber25 = variableNumber11.var_num;
				assert(variableNumber25);
				if variableNumber25 > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresHyperbolicCosine.args == 0 then
						return "NEVER";
					else
						return "hypcosine";
					end;
				elseif settings.smart_var_level >= 2 then
					return "hypcosine";
				else
					return "num";
				end;
			end, 
			["math.acos"] = function(variableNumber12, requiresArcCosine)
				local variableNumber26 = variableNumber12.var_num;
				assert(variableNumber26);
				if variableNumber26 > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresArcCosine.args == 0 then
						return "NEVER";
					else
						return "arccosine";
					end;
				elseif settings.smart_var_level >= 2 then
					return "arccosine";
				else
					return "num";
				end;
			end, 
			["math.tan"] = function(variableNumber13, requiresTangent)
				local variableNumber27 = variableNumber13.var_num;
				assert(variableNumber27);
				if variableNumber27 > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresTangent.args == 0 then
						return "NEVER";
					else
						return "tangent";
					end;
				elseif settings.smart_var_level >= 2 then
					return "tangent";
				else
					return "num";
				end;
			end, 
			["math.tanh"] = function(variableNumber14, requiresHyperbolicTangent)
				local hyptangentThreshold = variableNumber14.var_num;
				assert(hyptangentThreshold);
				if hyptangentThreshold > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresHyperbolicTangent.args == 0 then
						return "NEVER";
					else
						return "hyptangent";
					end;
				elseif settings.smart_var_level >= 2 then
					return "hyptangent";
				else
					return "num";
				end;
			end, 
			["math.atan"] = function(variableNumber15, requiresArcTangent)
				local arctangentThreshold = variableNumber15.var_num;
				assert(arctangentThreshold);
				if arctangentThreshold > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresArcTangent.args == 0 then
						return "NEVER";
					else
						return "arctangent";
					end;
				elseif settings.smart_var_level >= 2 then
					return "arctangent";
				else
					return "num";
				end;
			end, 
			["math.atan2"] = function(variableNumber16, requiresArcTangent2)
				local arctangentThreshold2 = variableNumber16.var_num;
				assert(arctangentThreshold2);
				if arctangentThreshold2 > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresArcTangent2.args < 2 then
						return "NEVER";
					else
						return "arctangent";
					end;
				elseif settings.smart_var_level >= 2 then
					return "arctangent";
				else
					return "num";
				end;
			end, 
			["math.rad"] = function(variableNumber17, requiresRadians)
				local radiansThreshold = variableNumber17.var_num;
				assert(radiansThreshold);
				if radiansThreshold > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresRadians.args == 0 then
						return "NEVER";
					else
						return "radians";
					end;
				elseif settings.smart_var_level >= 2 then
					return "radians";
				else
					return "num";
				end;
			end, 
			["math.deg"] = function(variableNumber18, requiresDegrees)
				local degreesThreshold = variableNumber18.var_num;
				assert(degreesThreshold);
				if degreesThreshold > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresDegrees.args == 0 then
						return "NEVER";
					else
						return "degrees";
					end;
				elseif settings.smart_var_level >= 2 then
					return "degrees";
				else
					return "num";
				end;
			end, 
			["math.sign"] = function(variableNumber19, requiresSign)
				local signThreshold = variableNumber19.var_num;
				assert(signThreshold);
				if signThreshold > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresSign.args == 0 then
						return "NEVER";
					else
						return "sign";
					end;
				elseif settings.smart_var_level >= 2 then
					return "sign";
				else
					return "num";
				end;
			end, 
			["math.exp"] = function(variableNumber20, requiresExponentiation)
				local exponentiatedThreshold = variableNumber20.var_num;
				assert(exponentiatedThreshold);
				if exponentiatedThreshold > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresExponentiation.args == 0 then
						return "NEVER";
					else
						return "exponentiated";
					end;
				else
					return "num";
				end;
			end, 
			["math.clamp"] = function(variableNumber21, requiresClamping)
				local clampedThreshold = variableNumber21.var_num;
				assert(clampedThreshold);
				if clampedThreshold > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresClamping.args < 3 then
						return "NEVER";
					else
						return "clamped";
					end;
				elseif settings.smart_var_level >= 2 then
					return "clamped";
				else
					return "num";
				end;
			end, 
			["math.min"] = function(variableNumber22, requiresMinimum)
				local minimumThreshold = variableNumber22.var_num;
				assert(minimumThreshold);
				if minimumThreshold > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresMinimum.args == 0 then
						return "NEVER";
					else
						return "minimum";
					end;
				elseif settings.smart_var_level >= 2 then
					return "minimum";
				else
					return "num";
				end;
			end, 
			["math.max"] = function(variableNumber23, requiresMaximum)
				local maximumThreshold = variableNumber23.var_num;
				assert(maximumThreshold);
				if maximumThreshold > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresMaximum.args == 0 then
						return "NEVER";
					else
						return "maximum";
					end;
				elseif settings.smart_var_level >= 2 then
					return "maximum";
				else
					return "num";
				end;
			end, 
			["Random.new"] = function(variableNumber24, _)
				local variableNumber = variableNumber24.var_num;
				assert(variableNumber);
				if variableNumber > 1 then
					return disableSmartVariables;
				else
					return "random_state";
				end;
			end, 
			["bit32.lshift"] = function(variableNumber25, requiresLeftShift)
				local leftShiftedThreshold = variableNumber25.var_num;
				assert(leftShiftedThreshold);
				if leftShiftedThreshold > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresLeftShift.args < 2 then
						return "NEVER";
					else
						return "lshifted";
					end;
				elseif settings.smart_var_level >= 2 then
					return "shifted";
				else
					return "num";
				end;
			end, 
			["bit32.rshift"] = function(variableNumber26, requiresRightShift)
				local rightShiftedThreshold = variableNumber26.var_num;
				assert(rightShiftedThreshold);
				if rightShiftedThreshold > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresRightShift.args < 2 then
						return "NEVER";
					else
						return "rshifted";
					end;
				elseif settings.smart_var_level >= 2 then
					return "shifted";
				else
					return "num";
				end;
			end, 
			["bit32.arshift"] = function(variableNumber27, requiresArithmeticRightShift)
				local arithmeticRightShiftedThreshold = variableNumber27.var_num;
				assert(arithmeticRightShiftedThreshold);
				if arithmeticRightShiftedThreshold > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresArithmeticRightShift.args < 2 then
						return "NEVER";
					else
						return "arshifted";
					end;
				elseif settings.smart_var_level >= 2 then
					return "shifted";
				else
					return "num";
				end;
			end, 
			["bit32.lrotate"] = function(variableNumber28, requiresLeftRotation)
				local leftRotatedThreshold = variableNumber28.var_num;
				assert(leftRotatedThreshold);
				if leftRotatedThreshold > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresLeftRotation.args < 2 then
						return "NEVER";
					else
						return "lrotated";
					end;
				elseif settings.smart_var_level >= 2 then
					return "rotated";
				else
					return "num";
				end;
			end, 
			["bit32.rrotate"] = function(variableNumber29, requiresRightRotation)
				local rightRotatedThreshold = variableNumber29.var_num;
				assert(rightRotatedThreshold);
				if rightRotatedThreshold > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresRightRotation.args < 2 then
						return "NEVER";
					else
						return "rrotated";
					end;
				elseif settings.smart_var_level >= 2 then
					return "rotated";
				else
					return "num";
				end;
			end, 
			["bit32.band"] = function(unknownObject1, requiresBit32MaxOrMasked)
				local maskedThreshold = unknownObject1.var_num;
				assert(maskedThreshold);
				if maskedThreshold > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresBit32MaxOrMasked.args == 0 then
						return "bit32_max";
					else
						return "masked";
					end;
				elseif settings.smart_var_level >= 2 then
					return "masked";
				else
					return "num";
				end;
			end, 
			["bit32.bor"] = function(unknownObject2, requiresZeroOrFlags)
				local flagsThreshold = unknownObject2.var_num;
				assert(flagsThreshold);
				if flagsThreshold > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresZeroOrFlags.args == 0 then
						return "zero";
					else
						return "flags";
					end;
				elseif settings.smart_var_level >= 2 then
					return "flags";
				else
					return "num";
				end;
			end, 
			["bit32.bxor"] = function(unknownObject3, requiresZeroOrXor)
				local xoredThreshold = unknownObject3.var_num;
				assert(xoredThreshold);
				if xoredThreshold > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresZeroOrXor.args == 0 then
						return "zero";
					else
						return "xored";
					end;
				elseif settings.smart_var_level >= 2 then
					return "xored";
				else
					return "num";
				end;
			end, 
			["bit32.bnot"] = function(unknownObject4, requiresInversion)
				local invertedThreshold = unknownObject4.var_num;
				assert(invertedThreshold);
				if invertedThreshold > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresInversion.args == 0 then
						return "NEVER";
					else
						return "inverted";
					end;
				elseif settings.smart_var_level >= 2 then
					return "inverted";
				else
					return "num";
				end;
			end, 
			["bit32.btest"] = function(unknownObject5, requiresTrueOrCommonBit)
				local foundCommonBitThreshold = unknownObject5.var_num;
				assert(foundCommonBitThreshold);
				if foundCommonBitThreshold > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresTrueOrCommonBit.args == 0 then
						return "true_";
					else
						return "found_common_bit";
					end;
				elseif settings.smart_var_level >= 2 then
					return "found_common_bit";
				else
					return "num";
				end;
			end, 
			["bit32.countlz"] = function(unknownObject6, requiresZeroCountLeft)
				local zeroCountLeftThreshold = unknownObject6.var_num;
				assert(zeroCountLeftThreshold);
				if zeroCountLeftThreshold > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresZeroCountLeft.args == 0 then
						return "NEVER";
					else
						return "zero_count_left";
					end;
				elseif settings.smart_var_level >= 2 then
					return "zero_count";
				else
					return "num";
				end;
			end, 
			["bit32.countrz"] = function(unknownObject7, requiresZeroCountRight)
				local zeroCountRightThreshold = unknownObject7.var_num;
				assert(zeroCountRightThreshold);
				if zeroCountRightThreshold > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresZeroCountRight.args == 0 then
						return "NEVER";
					else
						return "zero_count_right";
					end;
				elseif settings.smart_var_level >= 2 then
					return "zero_count";
				else
					return "num";
				end;
			end, 
			["bit32.extract"] = function(unknownObject8, requiresExtraction)
				local extractedThreshold = unknownObject8.var_num;
				assert(extractedThreshold);
				if extractedThreshold > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresExtraction.args < 2 then
						return "NEVER";
					else
						return "extracted";
					end;
				elseif settings.smart_var_level >= 2 then
					return "extracted";
				else
					return "num";
				end;
			end, 
			["bit32.replace"] = function(unknownObject9, requiresReplacement)
				local replacedThreshold = unknownObject9.var_num;
				assert(replacedThreshold);
				if replacedThreshold > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresReplacement.args < 3 then
						return "NEVER";
					else
						return "replaced";
					end;
				elseif settings.smart_var_level >= 2 then
					return "replaced";
				else
					return "num";
				end;
			end, 
			["debug.traceback"] = function(unknownObject10, _)
				local callStackThreshold = unknownObject10.var_num;
				assert(callStackThreshold);
				if callStackThreshold > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 2 then
					return "call_stack";
				else
					return "num";
				end;
			end, 
			["debug.info"] = function(v3679, argumentTable)
				if settings.smart_var_level >= 2 then
					if #argumentTable.args < 2 then
						return "NEVER";
					else
						local argument = if #argumentTable.args == 2 then argumentTable.args[2] else argumentTable.args[3];
						if argument.t == "constant" then
							local argumentConstant = argument.const;
							if argumentConstant.type == 3 then
								local constantValue = argumentConstant.value;
								if constantValue == "s" then
									local variableNumber = v3679.var_num;
									assert(variableNumber);
									if variableNumber == 1 then
										return "func_source";
									end;
								elseif constantValue == "l" then
									local lineNumberDefined = v3679.var_num;
									assert(lineNumberDefined);
									if lineNumberDefined == 1 then
										return "line_defined";
									end;
								elseif constantValue == "n" then
									local functionNameDefined = v3679.var_num;
									assert(functionNameDefined);
									if functionNameDefined == 1 then
										return "func_name";
									end;
								elseif constantValue == "a" then
									local variableNumber = v3679.var_num;
									assert(variableNumber);
									local variableNumberCopy = variableNumber;
									if variableNumberCopy == 1 then
										return "parameter_count";
									elseif variableNumberCopy == 2 then
										return "is_vararg";
									else
										return disableSmartVariables;
									end;
								elseif constantValue == "f" then
									local functionDefined = v3679.var_num;
									assert(functionDefined);
									if functionDefined == 1 then
										return "func";
									end;
								end;
							end;
							return "NEVER";
						else
							local infoDefined = v3679.var_num;
							assert(infoDefined);
							if infoDefined == 1 then
								return "info";
							end;
						end;
					end;
				else
					local numberDefined = v3679.var_num;
					assert(numberDefined);
					if numberDefined == 1 then
						return "num";
					end;
				end;
				return disableSmartVariables;
			end, 
			["Vector3.new"] = function(unknownObject11, requiresZeroVector3OrVector3)
				local vector3Threshold = unknownObject11.var_num;
				assert(vector3Threshold);
				if vector3Threshold > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresZeroVector3OrVector3.args == 0 then
						return "zero_vector3";
					else
						return "vector3";
					end;
				elseif settings.smart_var_level >= 2 then
					return "vector3";
				else
					return "vector";
				end;
			end, 
			["Vector2.new"] = function(unknownObject12, requiresZeroVector2OrVector2)
				local vector2Threshold = unknownObject12.var_num;
				assert(vector2Threshold);
				if vector2Threshold > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresZeroVector2OrVector2.args == 0 then
						return "zero_vector2";
					else
						return "vector2";
					end;
				elseif settings.smart_var_level >= 2 then
					return "vector2";
				else
					return "vector";
				end;
			end, 
			["UDim.new"] = function(unknownObject13, requiresZeroUDimOrUDim)
				local udimCheck = unknownObject13.var_num;
				assert(udimCheck);
				if udimCheck > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresZeroUDimOrUDim.args == 0 then
						return "zero_udim";
					else
						return "udim";
					end;
				else
					return "udim";
				end;
			end, 
			["UDim2.new"] = function(unknownObject14, requiresZeroUDim2OrUDim2_1)
				local udim2Check = unknownObject14.var_num;
				assert(udim2Check);
				if udim2Check > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresZeroUDim2OrUDim2_1.args == 0 then
						return "zero_udim2";
					else
						return "udim2";
					end;
				else
					return "udim2";
				end;
			end, 
			["UDim2.fromOffset"] = function(unknownObject15, requiresZeroUDim2OrUDim2_2)
				local udim2Check2 = unknownObject15.var_num;
				assert(udim2Check2);
				if udim2Check2 > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresZeroUDim2OrUDim2_2.args == 0 then
						return "zero_udim2";
					else
						return "udim2";
					end;
				else
					return "udim2";
				end;
			end, 
			["UDim2.fromScale"] = function(unknownObject16, requiresZeroUDim2OrUDim2_3)
				local udim2Check3 = unknownObject16.var_num;
				assert(udim2Check3);
				if udim2Check3 > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #requiresZeroUDim2OrUDim2_3.args == 0 then
						return "zero_udim2";
					else
						return "udim2";
					end;
				else
					return "udim2";
				end;
			end, 
			["CFrame.new"] = function(unknownObject17, requiresZeroCFrameOrCFrame)
				local cframeCheck = unknownObject17.var_num;
				assert(cframeCheck);
				if cframeCheck > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 and #requiresZeroCFrameOrCFrame.args == 0 then
					return "zero_cframe";
				else
					return "cframe";
				end;
			end, 
			["CFrame.Angles"] = function(unknownObject18, requiresCFrame_1)
				local cframeCheck2 = unknownObject18.var_num;
				assert(cframeCheck2);
				if cframeCheck2 > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 and #requiresCFrame_1.args < 3 then
					return "NEVER" .. (useExtensivePrefixes and "_cframe" or "");
				else
					return "cframe";
				end;
			end, 
			["CFrame.fromEulerAnglesXYZ"] = function(unknownObject19, requiresCFrame_2)
				local cframeCheck3 = unknownObject19.var_num;
				assert(cframeCheck3);
				if cframeCheck3 > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 and #requiresCFrame_2.args < 3 then
					return "NEVER" .. (useExtensivePrefixes and "_cframe" or "");
				else
					return "cframe";
				end;
			end, 
			["CFrame.fromEulerAnglesYXZ"] = function(unknownObject20, requiresCFrame_3)
				local cframeCheck4 = unknownObject20.var_num;
				assert(cframeCheck4);
				if cframeCheck4 > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 and #requiresCFrame_3.args < 3 then
					return "NEVER" .. (useExtensivePrefixes and "_cframe" or "");
				else
					return "cframe";
				end;
			end, 
			["CFrame.fromEulerAngles"] = function(unknownObject21, requiresCFrame_4)
				local cframeCheck5 = unknownObject21.var_num;
				assert(cframeCheck5);
				if cframeCheck5 > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 and #requiresCFrame_4.args < 3 then
					return "NEVER" .. (useExtensivePrefixes and "_cframe" or "");
				else
					return "cframe";
				end;
			end, 
			["CFrame.fromOrientation"] = function(unknownObject22, requiresCFrame_5)
				local cframeCheck6 = unknownObject22.var_num;
				assert(cframeCheck6);
				if cframeCheck6 > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 and #requiresCFrame_5.args < 3 then
					return "NEVER" .. (useExtensivePrefixes and "_cframe" or "");
				else
					return "cframe";
				end;
			end, 
			["CFrame.lookAt"] = function(unknownObject23, requiresCFrameLookingAt)
				local cframeLookingAtCheck = unknownObject23.var_num;
				assert(cframeLookingAtCheck);
				if cframeLookingAtCheck > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 and #requiresCFrameLookingAt.args < 2 then
					return "NEVER" .. (useExtensivePrefixes and "_cframe_looking_at" or "");
				elseif settings.smart_var_level >= 2 then
					return "cframe_looking_at";
				else
					return "cframe";
				end;
			end, 
			["CFrame.lookAlong"] = function(unknownObject24, requiresCFrameLookingAlong)
				local cframeLookingAlongCheck = unknownObject24.var_num;
				assert(cframeLookingAlongCheck);
				if cframeLookingAlongCheck > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 and #requiresCFrameLookingAlong.args < 2 then
					return "NEVER" .. (useExtensivePrefixes and "_cframe_looking_along" or "");
				elseif settings.smart_var_level >= 2 then
					return "cframe_looking_along";
				else
					return "cframe";
				end;
			end, 
			["CFrame.fromMatrix"] = function(unknownObject25, requiresCFrameMatrix)
				local cframeMatrixCheck = unknownObject25.var_num;
				assert(cframeMatrixCheck);
				if cframeMatrixCheck > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 and #requiresCFrameMatrix.args < 3 then
					return "NEVER" .. (useExtensivePrefixes and "_cframe_matrix" or "");
				elseif settings.smart_var_level >= 2 then
					return "cframe_matrix";
				else
					return "cframe";
				end;
			end, 
			["CFrame.fromAxisAngle"] = function(unknownObject26, requiresCFrameAxisAngle)
				local cframeAxisAngleCheck = unknownObject26.var_num;
				assert(cframeAxisAngleCheck);
				if cframeAxisAngleCheck > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 and #requiresCFrameAxisAngle.args < 2 then
					return "NEVER" .. (useExtensivePrefixes and "_cframe_axis_angle" or "");
				elseif settings.smart_var_level >= 2 then
					return "cframe_axis_angle";
				else
					return "cframe";
				end;
			end, 
			["game.GetService"] = function(unknownObject27, arguments13)
				local serviceArgs = unknownObject27.var_num;
				assert(serviceArgs);
				if serviceArgs > 1 then
					return disableSmartVariables;
				else
					local argsList = arguments13.args;
					if settings.smart_var_level >= 3 and #argsList == 0 then
						return "NEVER" .. (useExtensivePrefixes and "_service" or "");
					else
						serviceArgs = if settings.smart_var_level >= 3 then argsList else {
							argsList[1]
						};
						local isNever = nil;
						for _, arg in ipairs(serviceArgs) do
							if arg.t == "constant" and settings.smart_var_level >= 2 then
								local constantValue = arg.const;
								if constantValue.type == 3 then
									return constantValue.value .. (useExtensivePrefixes and "_service" or "");
								elseif settings.smart_var_level >= 3 then
									isNever = true;
								end;
							end;
						end;
						if isNever then
							return "NEVER" .. (useExtensivePrefixes and "_service" or "");
						elseif settings.smart_var_level >= 2 then
							return "service";
						else
							return;
						end;
					end;
				end;
			end, 
			select = function(unknownObject28, arguments14)
				local argumentCheck = unknownObject28.var_num;
				assert(argumentCheck);
				if argumentCheck > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 2 then
					local argsList2 = arguments14.args;
					argumentCheck = argsList2[1];
					if argumentCheck then
						if argumentCheck.t == "constant" then
							local argumentConstant = argumentCheck.const;
							if argumentConstant.type == 3 and argumentConstant.value == "#" then
								if #argsList2 == 2 and argsList2[2].t == "varargs" then
									return "arg_count";
								else
									return "len";
								end;
							end;
						end;
						if #argsList2 == 2 and argsList2[2].t == "varargs" then
							return "selected_arg";
						else
							return "selected";
						end;
					else
						return "NEVER";
					end;
				else
					return;
				end;
			end, 
			require = function(unknownObject29, argumentValue)
				local moduleArg = unknownObject29.var_num;
				assert(moduleArg);
				if moduleArg > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 2 then
					moduleArg = argumentValue.args[1];
					if moduleArg then
						local moduleName = nil;
						if not (moduleArg.t ~= "constant index") or moduleArg.t == "get table" then
							local _ = moduleArg.const;
							local modulePath, _ = getTableString(moduleArg);
							local _ = nil;
							local reversedString = string.reverse(modulePath);
							local dotIndex = string.find(reversedString, "%.");
							moduleName = if dotIndex then string.sub(modulePath, #modulePath - dotIndex + 2, #modulePath) else modulePath;
						elseif not (moduleArg.t ~= "name") or moduleArg.t == "global" then
							moduleName = if moduleArg.t == "name" then moduleArg.name.name else if moduleArg.name.type == 3 then moduleArg.name.value else "INVALIDGLOBAL";
						end;
						if moduleName then
							return (useExtensivePrefixes and "module_" or "") .. moduleName;
						elseif moduleArg.t == "constant" then
							local moduleConstant = moduleArg.const;
							if moduleConstant.type == 2 then
								return "external_module";
							elseif moduleConstant.type == 3 then
								return (useExtensivePrefixes and "module_" or "") .. sanitizeString(moduleConstant.value, true, true);
							end;
						else
							return "module";
						end;
					end;
					return "NEVER";
				else
					return;
				end;
			end, 
			["table.clear"] = function(_, _)
				return disableSmartVariables;
			end, 
			["table.foreach"] = function(_, _)
				return disableSmartVariables;
			end, 
			["table.foreachi"] = function(_, _)
				return disableSmartVariables;
			end, 
			["table.insert"] = function(_, _)
				return disableSmartVariables;
			end, 
			["table.sort"] = function(_, _)
				return disableSmartVariables;
			end, 
			["table.clone"] = function(unknownObject30, arguments31)
				local clonedTableCheck = unknownObject30.var_num;
				assert(clonedTableCheck);
				if clonedTableCheck > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #arguments31.args == 0 then
						return "NEVER";
					else
						return "cloned" .. (useExtensivePrefixes and "_tbl" or "");
					end;
				elseif settings.smart_var_level >= 2 then
					return "cloned";
				else
					return "tbl";
				end;
			end, 
			["table.concat"] = function(unknownObject32, arguments32)
				local concatenatedStringCheck = unknownObject32.var_num;
				assert(concatenatedStringCheck);
				if concatenatedStringCheck > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #arguments32.args < 2 then
						return "NEVER";
					else
						return "concatenated" .. (useExtensivePrefixes and "_tbl" or "");
					end;
				elseif settings.smart_var_level >= 2 then
					return "concatenated";
				else
					return "str";
				end;
			end, 
			["table.getn"] = function(unknownObject33, arguments33)
				local tableLengthCheck = unknownObject33.var_num;
				assert(tableLengthCheck);
				if tableLengthCheck > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #arguments33.args == 0 then
						return "NEVER";
					else
						return (useExtensivePrefixes and "tbl_" or "") .. "len";
					end;
				elseif settings.smart_var_level >= 2 then
					return "len";
				else
					return "num";
				end;
			end, 
			["table.maxn"] = function(unknownObject34, arguments34)
				local highestValueCheck = unknownObject34.var_num;
				assert(highestValueCheck);
				if highestValueCheck > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #arguments34.args == 0 then
						return "NEVER";
					else
						return (useExtensivePrefixes and "tbl_" or "") .. "highest_value";
					end;
				elseif settings.smart_var_level >= 2 then
					return "highest_value";
				else
					return "num";
				end;
			end, 
			["table.isfrozen"] = function(unknownObject35, arguments35)
				local isFrozenCheck = unknownObject35.var_num;
				assert(isFrozenCheck);
				if isFrozenCheck > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #arguments35.args == 0 then
						return "NEVER";
					else
						return (useExtensivePrefixes and "tbl_" or "") .. "is_frozen";
					end;
				elseif settings.smart_var_level >= 2 then
					return "is_frozen";
				else
					return "bool";
				end;
			end, 
			["table.pack"] = function(unknownObject36, _)
				local smartVarLevelRequirement = unknownObject36.var_num;
				assert(smartVarLevelRequirement);
				if smartVarLevelRequirement > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					return "packed" .. (useExtensivePrefixes and "_tbl" or "");
				elseif settings.smart_var_level >= 2 then
					return "packed";
				else
					return "tbl";
				end;
			end, 
			["table.unpack"] = function(unknownObject37, _)
				if settings.smart_var_level >= 2 then
					local unpackedValuePrefix = "unpacked_value_";
					local smartVarLevelRequirement2 = unknownObject37.var_num;
					assert(smartVarLevelRequirement2);
					return unpackedValuePrefix .. smartVarLevelRequirement2;
				else
					return;
				end;
			end, 
			["table.remove"] = function(unknownObject38, arguments36)
				local smartVarLevelRequirement3 = unknownObject38.var_num;
				assert(smartVarLevelRequirement3);
				if smartVarLevelRequirement3 > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #arguments36.args == 0 then
						return "NEVER";
					elseif #arguments36.args == 1 then
						return "popped" .. (useExtensivePrefixes and "_last_value" or "");
					else
						return "popped" .. (useExtensivePrefixes and "_value" or "");
					end;
				elseif settings.smart_var_level >= 2 then
					return "popped";
				else
					return;
				end;
			end, 
			["table.move"] = function(unknownObject39, arguments37)
				local smartVarLevelRequirement4 = unknownObject39.var_num;
				assert(smartVarLevelRequirement4);
				if smartVarLevelRequirement4 > 1 then
					return disableSmartVariables;
				elseif settings.smart_var_level >= 3 then
					if #arguments37.args < 4 then
						return "NEVER";
					else
						return "moved" .. (useExtensivePrefixes and "_tbl" or "");
					end;
				elseif settings.smart_var_level >= 2 then
					return "moved";
				else
					return "tbl";
				end;
			end, 
			["coroutine.running"] = function(unknownObject40, _)
				local variableNumber = unknownObject40.var_num;
				assert(variableNumber);
				if variableNumber > 1 then
					return disableSmartVariables;
				else
					return "current_thread";
				end;
			end
		};
		if settings.smart_var_level >= 3 then
			local _ = function(key)
				local clonedValue = cloneFunction[key];
				assert(clonedValue);
				local dataMap = methodMap;
				local _ = nil;
				local reversedString = string.reverse(key);
				local dotIndex = string.find(reversedString, "%.");
				dataMap[if dotIndex then string.sub(key, #key - dotIndex + 2, #key) else key] = clonedValue;
			end;
			local _ = function(key2)
				local clonedValue2 = cloneFunction[key2];
				assert(clonedValue2);
				if settings.smart_var_level >= 4 then
					local dataMap2 = methodMap;
					local _ = nil;
					local reversedString = string.reverse(key2);
					local dotIndex2 = string.find(reversedString, "%.");
					dataMap2[if dotIndex2 then string.sub(key2, #key2 - dotIndex2 + 2, #key2) else key2] = clonedValue2;
					return;
				else
					local dataMap3 = methodMap;
					local _ = nil;
					local reversedString = string.reverse(key2);
					local dotIndex3 = string.find(reversedString, "%.");
					dataMap3[if dotIndex3 then string.sub(key2, #key2 - dotIndex3 + 2, #key2) else key2] = function(argument1, unknownValue_1)
						local functionResult = clonedValue2(argument1, unknownValue_1);
						if functionResult then
							if string.match(functionResult, "NEVER") then
								return;
							else
								return functionResult;
							end;
						else
							return;
						end;
					end;
					return;
				end;
			end;
			local getServiceFunction = cloneFunction["game.GetService"];
			assert(getServiceFunction);
			if settings.smart_var_level >= 4 then
				local _ = nil;
				local reversedGetServiceString = string.reverse("game.GetService");
				local dotIndex4 = string.find(reversedGetServiceString, "%.");
				methodMap[if dotIndex4 then string.sub("game.GetService", 15 - dotIndex4 + 2, 15) else "game.GetService"] = getServiceFunction;
			else
				local _ = nil;
				local reversedGetServiceString = string.reverse("game.GetService");
				local dotPosition = string.find(reversedGetServiceString, "%.");
				local cachedFunction = getServiceFunction --[[ copy: 114 -> 161 ]];
				methodMap[if dotPosition then string.sub("game.GetService", 15 - dotPosition + 2, 15) else "game.GetService"] = function(argument1, unknownValue_2)
					local serviceResult = cachedFunction(argument1, unknownValue_2);
					if serviceResult then
						if string.match(serviceResult, "NEVER") then
							return;
						else
							return serviceResult;
						end;
					else
						return;
					end;
				end;
			end;
		end;
		local callTable = nil;
		callTable = {
			call = function(arg1, argTable, _)
				local func = argTable.func;
				if not (func.t ~= "constant index") or func.t == "get table" then
					local functionName, isError = getTableString(func);
					if isError and settings.smart_var_level < 2 then
						return;
					else
						if settings.smart_var_level >= 2 then
							local dataMap4 = methodMap;
							local _ = nil;
							local reversedString = string.reverse(functionName);
							local dotIndex5 = string.find(reversedString, "%.");
							local functionFromMap = dataMap4[if dotIndex5 then string.sub(functionName, #functionName - dotIndex5 + 2, #functionName) else functionName];
							if functionFromMap then
								dataMap4 = functionFromMap(arg1, argTable);
								if dataMap4 then
									return dataMap4;
								end;
							end;
						end;
						local functionFromClone = cloneFunction[functionName];
						if functionFromClone then
							return functionFromClone(arg1, argTable);
						elseif settings.smart_var_level >= 3 then
							return sanitizeString(functionName, nil, true) .. "_result" .. (arg1.var_num or "");
						end;
					end;
				elseif not (func.t ~= "name") or func.t == "global" then
					local globalName = nil;
					globalName = if func.t == "name" then func.name.name else if func.name.type == 3 then func.name.value else "INVALIDGLOBAL";
					local functionFromClone2 = cloneFunction[globalName];
					if functionFromClone2 then
						return functionFromClone2(arg1, argTable);
					elseif settings.smart_var_level >= 3 then
						return sanitizeString(globalName, nil, true) .. "_result" .. (arg1.var_num or "");
					end;
				end;
			end, 
			["new table"] = function(reads, unknownObject41, _)
				local isSetTable = false;
				local initializers = unknownObject41.initializers;
				local initializerType = "";
				if settings.smart_var_level >= 2 then
					if #initializers == 0 then
						if next(initializers) then
							initializerType = useExtensivePrefixes and "_dict" or "";
						end;
					else
						initializerType = if next(initializers) then useExtensivePrefixes and "_mixed" or "" else useExtensivePrefixes and "_list" or "";
					end;
				end;
				local namePrefix = "";
				if settings.smart_var_usage_analysis then
					local isTableSet = true;
					for _, readEntry in ipairs(reads.reads) do
						if readEntry.t == "set table" then
							isTableSet = false;
							break;
						end;
					end;
					if isTableSet then
						namePrefix = useExtensivePrefixes and "const_" or "";
					end;
				end;
				for _, initializer in ipairs(initializers) do
					if initializer.t == "varargs" then
						if settings.smart_var_level >= 2 then
							initializerType = initializerType .. "_varargs";
						end;
						if #initializers == 1 then
							if settings.smart_var_level >= 3 then
								return "args_list";
							else
								return "args";
							end;
						else
							break;
						end;
					end;
				end;
				local readsList = reads.reads;
				local lastRead = readsList[#readsList];
				local isModule;
				if lastRead and lastRead.t == "return" then
					local values = {};
					local valuesList = lastRead.values;
					if settings.smart_var_level == 3 then
						isModule = true;
						isSetTable = true;
					elseif #values == 1 then
						values = valuesList;
					else
						isModule = nil;
						isSetTable = true;
					end;
				end;
				if not isSetTable then
					if not isSetTable then
						isModule = nil;
					end;
				end;
				isSetTable = false;
				if isModule then
					return namePrefix .. "module" .. initializerType;
				else
					return namePrefix .. "tbl" .. initializerType;
				end;
			end, 
			["constant index"] = function(_, index, _)
				if settings.smart_var_level >= 2 then
					return (sanitizeString(constantToString(index.index, settings.string_quotes_behavior, true)));
				else
					return;
				end;
			end, 
			["get table"] = function(_, index, _)
				if settings.smart_var_level >= 2 then
					local indexExpression = index.index;
					if indexExpression.t == "constant" then
						local formattedIndex = constantToString(indexExpression.const, settings.string_quotes_behavior, true);
						if indexExpression.const.type ~= 3 then
							formattedIndex = "_" .. formattedIndex;
						end;
						return (sanitizeString(formattedIndex));
					end;
				end;
			end, 
			constant = function(writeInfo, unknownModule, _)
				if not settings.smart_var_usage_analysis then
					return;
				else
					local hasSingleWrite = #writeInfo.writes <= 1;
					if hasSingleWrite then
						for _, read in ipairs(writeInfo.reads) do
							if read.t == "set table" then
								hasSingleWrite = false;
								break;
							end;
						end;
					end;
					if hasSingleWrite then
						local constantType = unknownModule.const.type;
						if constantType == 1 then
							return "const_boolean";
						elseif constantType == 6 then
							return "const_function";
						elseif constantType == 4 then
							return "const_import";
						elseif constantType == 0 then
							return "const_nil";
						elseif constantType == 3 then
							return "const_string";
						elseif constantType == 2 then
							return "const_number";
						elseif constantType == 5 then
							return "const_table";
						elseif constantType == 7 then
							return "const_vector";
						else
							return "const_unknown";
						end;
					else
						return;
					end;
				end;
			end, 
			length = function(_, unknownModule2, _)
				if settings.smart_var_level >= 2 then
					local rightHandSide = unknownModule2.rhs;
					while rightHandSide.t == "name" do
						local overrideExpression = rightHandSide.name.override_expr;
						if overrideExpression then
							rightHandSide = overrideExpression;
						else
							break;
						end;
					end;
					local rightHandSide8 = rightHandSide;
					if rightHandSide8.t == "name" then
						rightHandSide = rightHandSide8.name.name;
						if not (rightHandSide ~= "args") or rightHandSide == "args_list" then
							return "arg_count";
						end;
					elseif rightHandSide8.t == "new table" then
						rightHandSide = rightHandSide8.initializers;
						if #rightHandSide == 1 and rightHandSide[1].t == "varargs" then
							return "arg_count";
						end;
					end;
					return "len";
				else
					return;
				end;
			end, 
			global = function(_, unknownModule3, _)
				return (sanitizeString(constantToString(unknownModule3.name, settings.string_quotes_behavior, true)));
			end
		};
		local function _(unknownValue2, unknownValue)
			local handlerFunction = callTable[unknownValue.t];
			if handlerFunction then
				local cachedValue = handlerFunction(unknownValue2, unknownValue, true);
				if cachedValue then
					return cachedValue;
				end;
			end;
		end;
		for _, initializerData in pairs(table.clone(variableMap)) do
			local attributes = initializerData.attributes
			local variableName = initializerData.name
			local originalName = variableName
			local initializerExpression = initializerData.init_expr

			if initializerExpression then
				if settings.smart_var_level > 0 then
					local expressionHandler = callTable[initializerExpression.t]
					variableName = expressionHandler and expressionHandler(initializerData, initializerExpression, true) or nil or variableName
				end

				if variableName ~= originalName then
					function is_valid_identifier(name)
						return type(name) == "string" and name:match("^[a-zA-Z_][a-zA-Z0-9_]*$") ~= nil
					end

					if not is_valid_identifier(variableName) then
						variableName = originalName
					end

					local renamedVariableName = variableName
					local nameCounter = variableIndexMap[renamedVariableName]
					local incrementedName = renamedVariableName
					local nameIndex = nameCounter or 1

					while variableMap[incrementedName] or globalNameCache[incrementedName] do
						nameIndex = nameIndex + 1
						incrementedName = renamedVariableName .. "_" .. nameIndex
						if not is_valid_identifier(incrementedName) then
							break
						end
					end

					if is_valid_identifier(incrementedName) then
						variableIndexMap[renamedVariableName] = nameIndex
						variableName = incrementedName
					else
						variableName = originalName
					end
				end

				if attributes.is_upvalue and settings.mark_upvalues then
					local upvalueName = if settings.mark_upvalues == "extra"
						then if attributes.is_upvalue == "read"
						then variableName .. "_upvr"
						else variableName .. "_upvw"
					else variableName .. "_upv"

						local upvalueCounter = variableIndexMap[upvalueName]
						local incrementedUpvalueName = upvalueName
						local upvalueIndex = upvalueCounter or 1

						while variableMap[incrementedUpvalueName] or globalNameCache[incrementedUpvalueName] do
							upvalueIndex = upvalueIndex + 1
							incrementedUpvalueName = upvalueName .. "_" .. upvalueIndex
							if not is_valid_identifier(incrementedUpvalueName) then
								break
							end
						end

						if is_valid_identifier(incrementedUpvalueName) then
							variableIndexMap[upvalueName] = upvalueIndex
							variableName = incrementedUpvalueName
						end
					end

					local finalVariableName = variableName
					local variableName = initializerData.name
					if not variableMap[variableName] then
						error((("[write] Variable %* not allocated"):format(variableName)))
					end
					variableMap[variableName] = nil
					initializerData.name = finalVariableName
					variableMap[finalVariableName] = initializerData
					initializerData.attributes.renamed = true
				end;
			end;
			benchmarkTimer:end_benchmark("Smart Naming");
			benchmarkTimer:start_benchmark("Global AST To String");
			local decompiledCode = ("-- Decompiled with Konstant V%*, a fast Luau decompiler made in Luau by plusgiant5\n"):format("2.1");
			if type(decompiledScriptSource) ~= "string" and getScriptHashFunction then
				decompiledCode = decompiledCode .. ("-- Script hash: %*\n"):format((string.upper(getScriptHashFunction(decompiledScriptSource))));
			end;
			decompiledCode = (decompiledCode .. ("-- Decompiled on %*\n"):format((os.date("%Y-%m-%d %H:%M:%S")))) .. ("-- Luau version %*%*\n"):format(string.byte(decompiledScriptSource, 1, 1), if luauTypesVersion then (", Types version %*"):format(luauTypesVersion) else "");
			local indentationString = if settings.spaces_in_indentation then string.rep(" ", settings.spaces_in_indentation) else string.char(9);
			local function indentFunction(unknownString)
				return string.rep(indentationString, #unknownString / #indentationString + 1);
			end;
			local expressionHandlerFunction = nil;
			local processAstNodes = nil;
			local appendValueToString = nil;
			local outputLines = {};
			local totalOutputLength = 0;
			local function _()
				totalOutputLength = totalOutputLength + #decompiledCode;
				table.insert(outputLines, decompiledCode);
				decompiledCode = "";
			end;
			local function _(unknownValue, index)
				totalOutputLength = totalOutputLength + #unknownValue;
				table.insert(outputLines, index, unknownValue);
			end;
			local function _(index2)
				local previousOutputString = decompiledCode;
				totalOutputLength = totalOutputLength + #previousOutputString;
				table.insert(outputLines, index2, previousOutputString);
				decompiledCode = "";
			end;
			local function _()
				if #decompiledCode > 20 then
					totalOutputLength = totalOutputLength + #decompiledCode;
					table.insert(outputLines, decompiledCode);
					decompiledCode = "";
				end;
			end;
			totalOutputLength = totalOutputLength + #decompiledCode;
			table.insert(outputLines, decompiledCode);
			decompiledCode = "";
			local function _(unknownValue3)
				local typeString = nil;
				local dataType = unknownValue3.type;
				typeString = dataType == "table" and "{[any]: any}" or dataType == "function" and "_function_" or dataType == "thread" and "_thread_" or dataType == "userdata" and "_userdata_" or dataType == "invalid" and "_invalid_" or dataType;
				if unknownValue3.optional then
					return typeString .. "?";
				else
					return typeString;
				end;
			end;
			local function formatFunctionArguments(functionData, _)
				local isVararg = functionData.is_vararg;
				local argumentList = functionData.args;
				for argumentIndex, argumentData in ipairs(argumentList) do
					decompiledCode = decompiledCode .. argumentData.name;
					if argumentData.luau_type then
						local argumentString = decompiledCode;
						local colonSpace = ": ";
						local luauType = argumentData.luau_type;
						local typeString = nil;
						local luauDataType = luauType.type;
						typeString = luauDataType == "table" and "{[any]: any}" or luauDataType == "function" and "_function_" or luauDataType == "thread" and "_thread_" or luauDataType == "userdata" and "_userdata_" or luauDataType == "invalid" and "_invalid_" or luauDataType;
						decompiledCode = argumentString .. colonSpace .. if luauType.optional then typeString .. "?" else typeString;
					end;
					if argumentIndex < #argumentList then
						decompiledCode = decompiledCode .. ", ";
					end;
				end;
				if isVararg then
					if #argumentList > 0 then
						decompiledCode = decompiledCode .. ", ";
					end;
					decompiledCode = decompiledCode .. "...";
				end;
			end;
			local function _(operatorPrecedence)
				return operatorPrecedence and operatorPrecedence.precedence or -100;
			end;
			local function _(valueA, valueB)
				local type4 = valueA.t;
				if type4 ~= valueB.t then
					return false;
				elseif type4 == "constant" then
					return valueA.const.value == valueB.const.value;
				elseif type4 == "name" then
					return valueA.name == valueB.name;
				else
					return true;
				end;
			end;
			local function _(unknownValue4, unknownValue5)
				local overrideExpression = unknownValue4.override_expr;
				if overrideExpression then
					expressionHandlerFunction(overrideExpression, unknownValue5);
					return;
				else
					decompiledCode = decompiledCode .. unknownValue4.name;
					return;
				end;
			end;
			local _ = function(functionPrototype, unknownName, _)
				local separatorString = " -- ";
				local lineDefined = functionPrototype.line_defined;
				if lineDefined then
					if lineDefined == -1 then
						decompiledCode = decompiledCode .. " -- Internal function, doesn't exist in bytecode";
						separatorString = ", ";
					elseif settings.show_proto_line_defined then
						decompiledCode = decompiledCode .. " -- Line " .. lineDefined;
						separatorString = ", ";
					end;
				end;
				if functionPrototype.name_known and functionPrototype.name ~= unknownName then
					decompiledCode = decompiledCode .. separatorString .. "Named \"" .. functionPrototype.name .. "\"";
				end;
			end;
			local function formatUpvalues(functionData, indentation)
				if settings.show_proto_upvalues then
					local upvalueCount = functionData.upvalues_count;
					if upvalueCount > 0 then
						local indentationString = string.rep(indentationString, #indentation / #indentationString + 1);
						decompiledCode = decompiledCode .. indentation .. "--[[ Upvalues[" .. upvalueCount .. "]:\n";
						local upvalueList = functionData.upvalues;
						for upvalueIndex = 0, upvalueCount - 1 do
							decompiledCode = decompiledCode .. indentationString .. "[" .. upvalueIndex + 1 .. "]: " .. upvalueList[upvalueIndex].name.name .. " (";
							if upvalueList[upvalueIndex].name.name == functionData.name.name then
								decompiledCode = decompiledCode .. "self-reference, ";
							end;
							decompiledCode = decompiledCode .. upvalueList[upvalueIndex].access .. ")\n";
						end;
						decompiledCode = decompiledCode .. indentation .. "]]\n";
					end;
				end;
			end;
			local function processFunction(functionData, unknownString2)
				totalOutputLength = totalOutputLength + #decompiledCode;
				table.insert(outputLines, decompiledCode);
				decompiledCode = "";
				local indentationString = string.rep(indentationString, #unknownString2 / #indentationString + 1);
				formatUpvalues(functionData, indentationString);
				totalOutputLength = totalOutputLength + #decompiledCode;
				table.insert(outputLines, decompiledCode);
				decompiledCode = "";
				local tempTable = outputLines;
				local abstractSyntaxTree = functionData.ast;
				outputLines = table.create(#abstractSyntaxTree * 5);
				processAstNodes(abstractSyntaxTree, indentationString);
				table.move(outputLines, 1, #outputLines, #tempTable + 1, tempTable);
				outputLines = tempTable;
				totalOutputLength = totalOutputLength + #decompiledCode;
				table.insert(outputLines, decompiledCode);
				decompiledCode = "";
			end;
			local function formatVariableUsage(variableInfo, readCount, writesCount, _)
				if readCount > 0 then
					if writesCount == 0 then
						return variableInfo .. " reads: " .. readCount;
					else
						return variableInfo .. " reads: " .. readCount .. ", writes: " .. writesCount;
					end;
				elseif writesCount ~= 0 then
					return variableInfo .. " writes: " .. writesCount;
				else
					return variableInfo .. " unused";
				end;
			end;
			local function generateVariableReport(variableList, debugString)
				for index, variableData in ipairs(variableList) do
					local formattedString = decompiledCode;
					local variableHeader = #variableList == 1 and "-- Variable" or "-- Variables[" .. index .. "]";
					local readsCount = #variableData.reads;
					local writeCount = #variableData.writes - 1;
					decompiledCode = formattedString .. (if readsCount > 0 then if writeCount == 0 then variableHeader .. " reads: " .. readsCount else variableHeader .. " reads: " .. readsCount .. ", writes: " .. writeCount else if writeCount ~= 0 then variableHeader .. " writes: " .. writeCount else variableHeader .. " unused") .. "\n" .. debugString;
				end;
			end;
			local statementHandlers = {
				nothing = function(_, _)
					error("Shouldn't happen !!!!!!!!!!");
				end, 
				comment = function(textData, _)
					decompiledCode = decompiledCode .. "-- " .. textData.text;
				end, 
				["break"] = function(_, _)
					decompiledCode = decompiledCode .. "break";
				end, 
				continue = function(_, _)
					decompiledCode = decompiledCode .. "continue";
				end, 
				["unknown jump"] = function(jumpInstruction, _)
					local jumpTableEntry = constantMap[jumpInstruction];
					if jumpTableEntry then
						local destinationInfo = jumpTableEntry[jumpInstruction.destination];
						if destinationInfo then
							decompiledCode = decompiledCode .. ("-- %*: GOTO [%*] #%*"):format(prefixWarning, destinationInfo.actual_code_index, destinationInfo.actual_index);
							return;
						else
							decompiledCode = decompiledCode .. ("-- %*: GOTO UNK2"):format(prefixWarning);
							return;
						end;
					else
						decompiledCode = decompiledCode .. ("-- %*: GOTO UNK1"):format(prefixWarning);
						return;
					end;
				end, 
				["if"] = function(ifStatement, indentation)
					decompiledCode = decompiledCode .. "if ";
					expressionHandlerFunction(ifStatement.expr, indentation);
					local newIndentation = string.rep(indentationString, #indentation / #indentationString + 1);
					local shortCircuitReturn = nil;
					if settings.minify_if_statements and not ifStatement.else_ and #ifStatement.elseifs == 0 and #ifStatement.pass == 1 then
						local passStatement = ifStatement.pass[1];
						local type5 = passStatement.t;
						if type5 == "return" then
							if #passStatement.values == 0 then
								shortCircuitReturn = type5;
							end;
						elseif not (type5 ~= "continue") or type5 == "break" then
							shortCircuitReturn = type5;
						end;
					end;
					if shortCircuitReturn then
						decompiledCode = decompiledCode .. " then " .. shortCircuitReturn .. " end";
						return;
					else
						decompiledCode = decompiledCode .. " then\n";
						processAstNodes(ifStatement.pass, newIndentation);
						for _, elseifStatement in ipairs(ifStatement.elseifs) do
							decompiledCode = decompiledCode .. indentation .. "elseif ";
							expressionHandlerFunction(elseifStatement.expr, indentation);
							decompiledCode = decompiledCode .. " then\n";
							processAstNodes(elseifStatement.code, newIndentation);
						end;
						if ifStatement.else_ then
							decompiledCode = decompiledCode .. indentation .. "else\n";
							processAstNodes(ifStatement.else_, newIndentation);
						end;
						decompiledCode = decompiledCode .. indentation .. "end";
						return;
					end;
				end, 
				["for"] = function(forStatement, currentIndentationLevel)
					local isStepValid = false;
					local forInfo = forStatement.for_info;
					local loopVariables = forInfo.variables;
					assert(loopVariables);
					if settings.mark_reads_and_writes then
						generateVariableReport(loopVariables, currentIndentationLevel);
					end;
					local variableCount = #loopVariables;
					decompiledCode = decompiledCode .. "for ";
					for variableIndex, variable in ipairs(loopVariables) do
						decompiledCode = decompiledCode .. variable.name;
						if variableIndex ~= variableCount then
							decompiledCode = decompiledCode .. ", ";
						end;
					end;
					local forLoopType = forInfo.type;
					if forLoopType == "numeric" then
						decompiledCode = decompiledCode .. " = ";
						local numericForArgs = forInfo.args;
						assert(numericForArgs.index_expr);
						assert(numericForArgs.end_expr);
						assert(numericForArgs.step_expr);
						expressionHandlerFunction(numericForArgs.index_expr, currentIndentationLevel);
						decompiledCode = decompiledCode .. ", ";
						expressionHandlerFunction(numericForArgs.end_expr, currentIndentationLevel);
						local stepExpression = numericForArgs.step_expr;
						while true do
							if not (stepExpression.t == "name") then
								break;
							end;
							local overrideExpression = stepExpression.name.override_expr;
							if overrideExpression then
								stepExpression = overrideExpression;
							else
								break;
							end;
						end;
						local cachedStepExpression = stepExpression;
						assert(cachedStepExpression);
						if cachedStepExpression.t == "constant" then
							stepExpression = cachedStepExpression.const;
							isStepValid = not (not (stepExpression.type == 2) or stepExpression.value ~= 1);
						end;
						if not isStepValid then
							decompiledCode = decompiledCode .. ", ";
							expressionHandlerFunction(cachedStepExpression, currentIndentationLevel);
						end;
					elseif forLoopType == "generic" then
						decompiledCode = decompiledCode .. " in ";
						local genericForArgs = forInfo.args;
						local generatorExpression = genericForArgs.generator_expr;
						assert(generatorExpression);
						local generatorArguments = nil;
						local stateExpression = genericForArgs.state_expr;
						local indexExpression = genericForArgs.index_expr;
						local isConditionMet = nil;
						if not indexExpression or indexExpression.t == "nil" then
							isConditionMet = if not stateExpression or stateExpression.t == "nil" then not expressionTypes[generatorExpression.t] else not expressionTypes[stateExpression.t];
						end;
						generatorArguments = if indexExpression then if stateExpression then if stateExpression.t == "nil" and indexExpression.t == "nil" then {
								generatorExpression, 
								stateExpression
								} else {
									generatorExpression, 
									stateExpression, 
									indexExpression
								} else {
								generatorExpression, 
								createDefaultMetadata(), 
								indexExpression
								} else if stateExpression then {
								generatorExpression, 
								stateExpression
								} else {
								generatorExpression
								};
								if isConditionMet then
									while true do
										if not (#generatorArguments > 1 and generatorArguments[#generatorArguments].t == "nil") then
											break;
										end;
										table.remove(generatorArguments);
									end;
								end;
								local arrayLength = #generatorArguments;
								for index, element in ipairs(generatorArguments) do
									if type(element) == "number" then
										decompiledCode = decompiledCode .. "nil";
									else
										expressionHandlerFunction(element, currentIndentationLevel);
									end;
									if index < arrayLength then
										decompiledCode = decompiledCode .. ", ";
									end;
								end;
					else
						error((("Unknown for_type \"%*\""):format(forLoopType)));
					end;
					isStepValid = false;
					decompiledCode = decompiledCode .. " do\n";
					processAstNodes(forStatement.code, indentFunction(currentIndentationLevel));
					decompiledCode = decompiledCode .. currentIndentationLevel .. "end";
				end, 
				["while"] = function(whileLoopData, whileLoopContext)
					if whileLoopData.expr then
						decompiledCode = decompiledCode .. "while ";
						expressionHandlerFunction(whileLoopData.expr, whileLoopContext);
						decompiledCode = decompiledCode .. " do\n";
					elseif settings.do_while_1 then
						decompiledCode = decompiledCode .. "while 1 do\n";
					else
						decompiledCode = decompiledCode .. "while true do\n";
					end;
					processAstNodes(whileLoopData.code, indentFunction(whileLoopContext));
					decompiledCode = decompiledCode .. whileLoopContext .. "end";
				end, 
				["repeat"] = function(repeatLoopData, repeatLoopContext)
					decompiledCode = decompiledCode .. "repeat\n";
					processAstNodes(repeatLoopData.code, indentFunction(repeatLoopContext));
					if repeatLoopData.expr then
						decompiledCode = decompiledCode .. repeatLoopContext .. "until ";
						expressionHandlerFunction(repeatLoopData.expr, repeatLoopContext);
						return;
					else
						decompiledCode = decompiledCode .. repeatLoopContext .. "until nil";
						return;
					end;
				end, 
				["do"] = function(content, endString)
					decompiledCode = decompiledCode .. "do\n";
					processAstNodes(content.content, indentFunction(endString));
					decompiledCode = decompiledCode .. endString .. "end";
				end, 
				call = function(functionData, expressionContext)
					local currentFunction = functionData.func;
					while currentFunction.t == "name" do
						local overrideExpression = currentFunction.name.override_expr;
						if overrideExpression then
							currentFunction = overrideExpression;
						else
							break;
						end;
					end;
					local tempFunction = currentFunction;
					if tempFunction.t == "constant index" then
						currentFunction = tempFunction.table;
						if currentFunction.t == "constant" then
							local constantData = currentFunction.const;
							if constantData.type == 3 then
								local function _(formattedString)
									return (string.gsub(formattedString, "%%%%", "%%"));
								end;
								local constantValue = constantData.value;
								local bufferData = buffer.fromstring(constantValue);
								local stringPartsList = {};
								local startIndex = 1;
								local isEscapeSequence = false;
								for charIndex = 0, #constantValue - 1 do
									local charCode = buffer.readu8(bufferData, charIndex);
									if charCode == 37 then
										isEscapeSequence = not isEscapeSequence;
									else
										if charCode == 42 and isEscapeSequence then
											local substringBefore = string.sub(constantValue, startIndex, charIndex - 1);
											table.insert(stringPartsList, (string.gsub(substringBefore, "%%%%", "%%")));
											startIndex = charIndex + 2;
										end;
										isEscapeSequence = false;
									end;
								end;
								local substringAfter = string.sub(constantValue, startIndex, #constantValue);
								table.insert(stringPartsList, (string.gsub(substringAfter, "%%%%", "%%")));
								if #stringPartsList - 1 == #functionData.args then
									decompiledCode = decompiledCode .. "`";
									totalOutputLength = totalOutputLength + #decompiledCode;
									table.insert(outputLines, decompiledCode);
									decompiledCode = "";
									for argIndex, stringPart in ipairs(stringPartsList) do
										decompiledCode = decompiledCode .. stringQuoteHandler(settings.string_quotes_behavior, stringPart, true);
										totalOutputLength = totalOutputLength + #decompiledCode;
										table.insert(outputLines, decompiledCode);
										decompiledCode = "";
										if argIndex < #stringPartsList then
											decompiledCode = decompiledCode .. "{";
											expressionHandlerFunction(functionData.args[argIndex], expressionContext);
											decompiledCode = decompiledCode .. "}";
											totalOutputLength = totalOutputLength + #decompiledCode;
											table.insert(outputLines, decompiledCode);
											decompiledCode = "";
										end;
									end;
									decompiledCode = decompiledCode .. "`";
									return;
								end;
							end;
						end;
					end;
					currentFunction = expressionHandlerFunction;
					local tempFunction2 = tempFunction;
					local expressionContextLocal = expressionContext;
					local isConstantOrHasPrecedence = true;
					if tempFunction.t ~= "concatenation" then
						isConstantOrHasPrecedence = true;
						if tempFunction.t ~= "function" then
							isConstantOrHasPrecedence = tempFunction.t == "constant" and tempFunction.const.type ~= 4 or (tempFunction and tempFunction.precedence or -100) >= 0;
						end;
					end;
					currentFunction(tempFunction2, expressionContextLocal, isConstantOrHasPrecedence);
					if functionData.namecall_method then
						decompiledCode = decompiledCode .. ":" .. functionData.namecall_method .. "(";
					else
						decompiledCode = decompiledCode .. "(";
					end;
					for argumentIndex, argumentValue in ipairs(functionData.args) do
						expressionHandlerFunction(argumentValue, expressionContext);
						if argumentIndex < #functionData.args then
							decompiledCode = decompiledCode .. ", ";
						end;
					end;
					decompiledCode = decompiledCode .. ")";
				end, 
				["define variable"] = function(variableData, variableContext)
					if settings.mark_reads_and_writes then
						generateVariableReport(variableData.names, variableContext);
					end;
					decompiledCode = decompiledCode .. "local ";
					for nameIndex, nameData in ipairs(variableData.names) do
						decompiledCode = decompiledCode .. nameData.name;
						if nameIndex < #variableData.names then
							decompiledCode = decompiledCode .. ", ";
						end;
					end;
					if (settings.show_nil_definitions or variableData.value.t ~= "nil") and not variableData.value.invisible then
						decompiledCode = decompiledCode .. " = ";
						expressionHandlerFunction(variableData.value, variableContext);
					end;
				end, 
				["define function"] = function(debugInfo, indentationLevel)
					local functionData = debugInfo.func;
					local functionType = debugInfo.define_function_type;
					if settings.mark_reads_and_writes and functionType == "local" then
						generateVariableReport({
							functionData.varname
						}, indentationLevel);
					end;
					if functionType == "local" then
						decompiledCode = decompiledCode .. "local function " .. debugInfo.func_name.name .. "(";
					elseif functionType == "table" then
						decompiledCode = decompiledCode .. "function ";
						local functionPath = debugInfo.path;
						for pathIndex, pathComponent in ipairs(functionPath) do
							if pathIndex == #functionPath then
								decompiledCode = decompiledCode .. pathComponent.name;
							else
								decompiledCode = decompiledCode .. pathComponent.name .. ".";
							end;
						end;
						decompiledCode = decompiledCode .. "(";
					else
						decompiledCode = decompiledCode .. "function " .. debugInfo.func_name.name .. "(";
					end;
					formatFunctionArguments(functionData, indentationLevel);
					decompiledCode = decompiledCode .. ")";
					local functionName = debugInfo.func_name.name;
					local namedSeparator = " -- ";
					local functionLineDefined = functionData.line_defined;
					if functionLineDefined then
						if functionLineDefined == -1 then
							decompiledCode = decompiledCode .. " -- Internal function, doesn't exist in bytecode";
							namedSeparator = ", ";
						elseif settings.show_proto_line_defined then
							decompiledCode = decompiledCode .. " -- Line " .. functionLineDefined;
							namedSeparator = ", ";
						end;
					end;
					if functionData.name_known and functionData.name ~= functionName then
						decompiledCode = decompiledCode .. namedSeparator .. "Named \"" .. functionData.name .. "\"";
					end;
					decompiledCode = decompiledCode .. "\n";
					processFunction(functionData, indentationLevel);
					decompiledCode = decompiledCode .. indentationLevel .. "end";
				end, 
				["set variable"] = function(expression, environment)
					decompiledCode = decompiledCode .. expression.name.name;
					local assignmentOperator = compoundAssignmentOperators[expression.value.t];
					local isRhsPresent = false;
					if assignmentOperator and settings.use_compound_assignment then
						if expression.value.t == "concatenation" then
							if #expression.value.exprs > 1 and expression.value.exprs[1].t == "global" and expression.value.exprs[1].name == expression.name then
								isRhsPresent = true;
							end;
						elseif expression.value.lhs.t == "name" and expression.name.name == expression.value.lhs.name.name then
							isRhsPresent = true;
						end;
					end;
					if isRhsPresent then
						decompiledCode = decompiledCode .. " " .. assignmentOperator .. " ";
						expressionHandlerFunction(expression.value.rhs, environment);
						return;
					else
						decompiledCode = decompiledCode .. " = ";
						expressionHandlerFunction(expression.value, environment);
						return;
					end;
				end, 
				["set global"] = function(expression, context)
					local _ = expression.value;
					decompiledCode = decompiledCode .. expression.name.value;
					local assignmentOperator = compoundAssignmentOperators[expression.value.t];
					local isRhsPresentForConcatenation = false;
					if assignmentOperator and settings.use_compound_assignment then
						if expression.value.t == "concatenation" then
							if #expression.value.exprs > 1 and expression.value.exprs[1].t == "global" and expression.value.exprs[1].name == expression.name then
								isRhsPresentForConcatenation = true;
							end;
						elseif expression.value.lhs.t == "constant" and expression.name.value == expression.value.lhs.const.value then
							isRhsPresentForConcatenation = true;
						end;
					end;
					if isRhsPresentForConcatenation then
						decompiledCode = decompiledCode .. " " .. assignmentOperator .. " ";
						if expression.value.t == "concatenation" then
							local clonedExpression = table.clone(expression.value);
							local expressionListClone = table.clone(clonedExpression.exprs);
							table.remove(expressionListClone, 1);
							clonedExpression.exprs = expressionListClone;
							expressionHandlerFunction(clonedExpression, context);
						else
							expressionHandlerFunction(expression.value.rhs, context);
						end;
					else
						decompiledCode = decompiledCode .. " = ";
						expressionHandlerFunction(expression.value, context);
					end;
					if settings.mark_setglobal then
						decompiledCode = decompiledCode .. " -- Setting global";
					end;
				end, 
				["set table"] = function(tableAccess, context)
					local currentTable = tableAccess.table;
					while currentTable.t == "name" do
						local overrideExpression = currentTable.name.override_expr;
						if overrideExpression then
							currentTable = overrideExpression;
						else
							break;
						end;
					end;
					local originalTable = currentTable;
					expressionHandlerFunction(originalTable, context, originalTable.t == "new table");
					if tableAccess.key.t == "constant" and tableAccess.key.const.type == 3 and isValidIdentifier(tableAccess.key.const.value) then
						decompiledCode = decompiledCode .. "." .. tableAccess.key.const.value .. " ";
					else
						decompiledCode = decompiledCode .. "[";
						expressionHandlerFunction(tableAccess.key, context);
						decompiledCode = decompiledCode .. "] ";
					end;
					currentTable = compoundAssignmentOperators[tableAccess.value.t];
					local isTablePresent = false;
					if currentTable and settings.use_compound_assignment then
						local indexValue = nil;
						if tableAccess.value.t == "concatenation" then
							if originalTable.t == "name" and tableAccess.value.exprs[1].t == "get table" and tableAccess.value.exprs[1].table.t == "name" and originalTable.name == tableAccess.value.exprs[1].table.name then
								indexValue = tableAccess.value.exprs[1].index;
							end;
						elseif originalTable.t == "name" and tableAccess.value.lhs.t == "get table" and tableAccess.value.lhs.table.t == "name" and originalTable.name == tableAccess.value.lhs.table.name then
							indexValue = tableAccess.value.lhs.index;
						end;
						if indexValue then
							local expressionValue = indexValue;
							local keyValue = tableAccess.key;
							local expressionType = expressionValue.t;
							if expressionType == keyValue.t and if expressionType == "constant" then expressionValue.const.value == keyValue.const.value else not (expressionType == "name") or expressionValue.name == keyValue.name then
								isTablePresent = true;
							end;
						end;
					end;
					if isTablePresent then
						decompiledCode = decompiledCode .. currentTable .. " ";
						if tableAccess.value.t == "concatenation" then
							local clonedExpression = table.clone(tableAccess.value);
							local expressionListClone = table.clone(clonedExpression.exprs);
							table.remove(expressionListClone, 1);
							clonedExpression.exprs = expressionListClone;
							expressionHandlerFunction(clonedExpression, context);
							return;
						else
							expressionHandlerFunction(tableAccess.value.rhs, context);
							return;
						end;
					else
						decompiledCode = decompiledCode .. "= ";
						expressionHandlerFunction(tableAccess.value, context);
						return;
					end;
				end, 
				["return"] = function(valuesTable, valueIndex)
					decompiledCode = decompiledCode .. "return";
					local argumentList = valuesTable.values;
					local argumentCount = #argumentList;
					if argumentCount > 0 then
						decompiledCode = decompiledCode .. " ";
					end;
					for argumentIndex = 1, argumentCount do
						expressionHandlerFunction(argumentList[argumentIndex], valueIndex);
						if argumentIndex < argumentCount then
							decompiledCode = decompiledCode .. ", ";
						end;
					end;
				end
			};
			local expressionHandlers = {
				global = function(globalExpression, environment)
					if globalExpression.name.type == 3 then
						decompiledCode = decompiledCode .. globalExpression.name.value;
						return;
					elseif globalExpression.name.type == 4 then
						appendValueToString(globalExpression.name, environment);
						return;
					else
						error("Corrupted global");
						return;
					end;
				end, 
				name = function(nameTable, environment)
					local nameData = nameTable.name;
					local overrideExpression = nameData.override_expr;
					if overrideExpression then
						expressionHandlerFunction(overrideExpression, environment);
						return;
					else
						decompiledCode = decompiledCode .. nameData.name;
						return;
					end;
				end, 
				varargs = function(_, _)
					decompiledCode = decompiledCode .. "...";
				end, 
				["nil"] = function(_, _)
					decompiledCode = decompiledCode .. "nil";
				end, 
				boolean = function(booleanValue, _)
					decompiledCode = decompiledCode .. (booleanValue.value and "true" or "false");
				end, 
				["new table"] = function(initializerData, repeatedString)
					local initializers = initializerData.initializers;
					local useTableKeys = true;
					for _ in pairs(initializers) do
						useTableKeys = false;
						break;
					end;
					if useTableKeys then
						decompiledCode = decompiledCode .. "{}";
					else
						decompiledCode = decompiledCode .. "{";
						local initializerCount = table.maxn(initializers);
						local hasNonNumericKeys = false;
						if settings.always_use_table_keys then
							hasNonNumericKeys = true;
						else
							for initializerKey in pairs(initializers) do
								if type(initializerKey) ~= "number" then
									hasNonNumericKeys = true;
									break;
								end;
							end;
						end;
						local isConditionMet = hasNonNumericKeys;
						local tempValue = repeatedString;
						if isConditionMet then
							repeatedString = string.rep(indentationString, #repeatedString / #indentationString + 1);
							decompiledCode = decompiledCode .. "\n" .. repeatedString;
						end;
						local initializersOrderList = initializerData.initializers_order;
						local initializerCount = #initializersOrderList;
						for initializerIndex, initializerKey in ipairs(initializersOrderList) do
							local initializerValue = initializers[initializerKey];
							if type(initializerKey) == "table" then
								if settings.table_string_key_shortening and initializerKey.t == "constant" and initializerKey.const.type == 3 and isValidIdentifier(initializerKey.const.value) then
									decompiledCode = decompiledCode .. initializerKey.const.value .. " = ";
								else
									decompiledCode = decompiledCode .. "[";
									expressionHandlerFunction(initializerKey, repeatedString);
									decompiledCode = decompiledCode .. "] = ";
								end;
								expressionHandlerFunction(initializerValue, repeatedString);
								if settings.table_dict_key_semicolons then
									decompiledCode = decompiledCode .. ";";
								else
									decompiledCode = decompiledCode .. ",";
								end;
								if initializerIndex ~= initializerCount then
									decompiledCode = decompiledCode .. "\n" .. repeatedString;
								end;
							elseif settings.always_use_table_keys then
								decompiledCode = decompiledCode .. "[" .. initializerKey .. "] = ";
								expressionHandlerFunction(initializerValue, repeatedString);
								if settings.table_dict_key_semicolons then
									decompiledCode = decompiledCode .. ";";
								else
									decompiledCode = decompiledCode .. ",";
								end;
								if initializerIndex ~= initializerCount then
									decompiledCode = decompiledCode .. "\n" .. repeatedString;
								end;
							end;
							if #decompiledCode > 20 then
								totalOutputLength = totalOutputLength + #decompiledCode;
								table.insert(outputLines, decompiledCode);
								decompiledCode = "";
							end;
						end;
						if not settings.always_use_table_keys then
							if hasNonNumericKeys and initializerCount > 0 then
								decompiledCode = decompiledCode .. "\n" .. repeatedString;
							end;
							for arrayIndex = 1, initializerCount do
								if not (arrayIndex % 10 == 0) or isConditionMet then

								end;
								expressionHandlerFunction(initializers[arrayIndex], repeatedString);
								if arrayIndex < initializerCount then
									if settings.table_array_value_semicolons then
										decompiledCode = decompiledCode .. "; ";
									else
										decompiledCode = decompiledCode .. ", ";
									end;
								end;
								if #decompiledCode > 20 then
									totalOutputLength = totalOutputLength + #decompiledCode;
									table.insert(outputLines, decompiledCode);
									decompiledCode = "";
								end;
							end;
						end;
						repeatedString = tempValue;
						if isConditionMet then
							decompiledCode = decompiledCode .. "\n" .. repeatedString;
						end;
						decompiledCode = decompiledCode .. "}";
					end;
					if #decompiledCode > 20 then
						totalOutputLength = totalOutputLength + #decompiledCode;
						table.insert(outputLines, decompiledCode);
						decompiledCode = "";
					end;
				end, 
				constant = function(constantData, environment)
					appendValueToString(constantData.const, environment);
				end, 
				call = statementHandlers.call, 
				["constant index"] = function(tableInfo, indentationLevel)
					expressionHandlerFunction(tableInfo.table, indentationLevel);
					if tableInfo.index.type == 3 and isValidIdentifier(tableInfo.index.value) then
						if tableInfo.namecall then
							decompiledCode = decompiledCode .. ":" .. tableInfo.index.value;
							return;
						else
							decompiledCode = decompiledCode .. "." .. tableInfo.index.value;
							return;
						end;
					else
						if tableInfo.namecall then
							decompiledCode = decompiledCode .. "--[[Namecall requested, but namecall method is an invalid name]]";
						end;
						decompiledCode = decompiledCode .. "[";
						appendValueToString(tableInfo.index, indentationLevel);
						decompiledCode = decompiledCode .. "]";
						return;
					end;
				end, 
				["function"] = function(expressionData, indentationString)
					decompiledCode = decompiledCode .. "function(";
					formatFunctionArguments(expressionData, indentationString);
					decompiledCode = decompiledCode .. ")";
					local namedSeparator = " -- ";
					local lineDefined = expressionData.line_defined;
					if lineDefined then
						if lineDefined == -1 then
							decompiledCode = decompiledCode .. " -- Internal function, doesn't exist in bytecode";
							namedSeparator = ", ";
						elseif settings.show_proto_line_defined then
							decompiledCode = decompiledCode .. " -- Line " .. lineDefined;
							namedSeparator = ", ";
						end;
					end;
					if expressionData.name_known and expressionData.name ~= nil then
						decompiledCode = decompiledCode .. namedSeparator .. "Named \"" .. expressionData.name .. "\"";
					end;
					decompiledCode = decompiledCode .. "\n";
					totalOutputLength = totalOutputLength + #decompiledCode;
					table.insert(outputLines, decompiledCode);
					decompiledCode = "";
					processFunction(expressionData, indentationString);
					decompiledCode = decompiledCode .. indentationString .. "end";
				end, 
				["and"] = function(binaryOperation, rhsValue)
					local printExpression0 = expressionHandlerFunction;
					local leftHandSide4 = binaryOperation.lhs;
					local shouldWrap4 = rhsValue;
					local leftHandSideExpression = binaryOperation.lhs;
					printExpression0(leftHandSide4, shouldWrap4, (leftHandSideExpression and leftHandSideExpression.precedence or -100) > (binaryOperation and binaryOperation.precedence or -100));
					decompiledCode = decompiledCode .. " and ";
					printExpression0 = expressionHandlerFunction;
					leftHandSide4 = binaryOperation.rhs;
					shouldWrap4 = rhsValue;
					leftHandSideExpression = binaryOperation.rhs;
					printExpression0(leftHandSide4, shouldWrap4, (leftHandSideExpression and leftHandSideExpression.precedence or -100) > (binaryOperation and binaryOperation.precedence or -100));
				end, 
				["or"] = function(binaryOperation, rhsValue)
					local printExpression1 = expressionHandlerFunction;
					local leftHandSide6 = binaryOperation.lhs;
					local shouldWrap6 = rhsValue;
					local leftHandSideExpression = binaryOperation.lhs;
					printExpression1(leftHandSide6, shouldWrap6, (leftHandSideExpression and leftHandSideExpression.precedence or -100) > (binaryOperation and binaryOperation.precedence or -100));
					decompiledCode = decompiledCode .. " or ";
					printExpression1 = expressionHandlerFunction;
					leftHandSide6 = binaryOperation.rhs;
					shouldWrap6 = rhsValue;
					leftHandSideExpression = binaryOperation.rhs;
					printExpression1(leftHandSide6, shouldWrap6, (leftHandSideExpression and leftHandSideExpression.precedence or -100) > (binaryOperation and binaryOperation.precedence or -100));
				end, 
				condition = function(conditionalExpression, rhsValue)
					local printExpression2 = expressionHandlerFunction;
					local leftHandSide8 = conditionalExpression.lhs;
					local shouldWrap8 = rhsValue;
					local leftHandSideExpression = conditionalExpression.lhs;
					printExpression2(leftHandSide8, shouldWrap8, (leftHandSideExpression and leftHandSideExpression.precedence or -100) > (conditionalExpression and conditionalExpression.precedence or -100));
					decompiledCode = decompiledCode .. " " .. conditionalExpression.condition .. " ";
					printExpression2 = expressionHandlerFunction;
					leftHandSide8 = conditionalExpression.rhs;
					shouldWrap8 = rhsValue;
					leftHandSideExpression = conditionalExpression.rhs;
					printExpression2(leftHandSide8, shouldWrap8, (leftHandSideExpression and leftHandSideExpression.precedence or -100) > (conditionalExpression and conditionalExpression.precedence or -100));
				end, 
				addition = function(additionExpression, rhsValue)
					local printExpression3 = expressionHandlerFunction;
					local leftHandSide10 = additionExpression.lhs;
					local shouldWrap10 = rhsValue;
					local isSubtraction = true;
					local leftHandSideExpression = additionExpression.lhs;
					if (leftHandSideExpression and leftHandSideExpression.precedence or -100) <= (additionExpression and additionExpression.precedence or -100) then
						isSubtraction = true;
						if additionExpression.rhs.t ~= "addition" then
							isSubtraction = additionExpression.rhs.t == "subtraction";
						end;
					end;
					printExpression3(leftHandSide10, shouldWrap10, isSubtraction);
					decompiledCode = decompiledCode .. " + ";
					printExpression3 = expressionHandlerFunction;
					leftHandSide10 = additionExpression.rhs;
					shouldWrap10 = rhsValue;
					isSubtraction = true;
					leftHandSideExpression = additionExpression.rhs;
					if (leftHandSideExpression and leftHandSideExpression.precedence or -100) <= (additionExpression and additionExpression.precedence or -100) then
						isSubtraction = true;
						if additionExpression.rhs.t ~= "addition" then
							isSubtraction = additionExpression.rhs.t == "subtraction";
						end;
					end;
					printExpression3(leftHandSide10, shouldWrap10, isSubtraction);
				end, 
				subtraction = function(additionExpression2, rhsValue)
					local printExpression4 = expressionHandlerFunction;
					local leftHandSide12 = additionExpression2.lhs;
					local shouldWrap12 = rhsValue;
					local isSubtraction = true;
					local leftHandSideExpression = additionExpression2.lhs;
					if (leftHandSideExpression and leftHandSideExpression.precedence or -100) <= (additionExpression2 and additionExpression2.precedence or -100) then
						isSubtraction = true;
						if additionExpression2.rhs.t ~= "addition" then
							isSubtraction = additionExpression2.rhs.t == "subtraction";
						end;
					end;
					printExpression4(leftHandSide12, shouldWrap12, isSubtraction);
					decompiledCode = decompiledCode .. " - ";
					printExpression4 = expressionHandlerFunction;
					leftHandSide12 = additionExpression2.rhs;
					shouldWrap12 = rhsValue;
					isSubtraction = true;
					leftHandSideExpression = additionExpression2.rhs;
					if (leftHandSideExpression and leftHandSideExpression.precedence or -100) <= (additionExpression2 and additionExpression2.precedence or -100) then
						isSubtraction = true;
						if additionExpression2.rhs.t ~= "addition" then
							isSubtraction = additionExpression2.rhs.t == "subtraction";
						end;
					end;
					printExpression4(leftHandSide12, shouldWrap12, isSubtraction);
				end, 
				multiplication = function(expression4174, rhsValue)
					local expressionFunction5 = expressionHandlerFunction;
					local leftHandSide14 = expression4174.lhs;
					local expressionValue4175 = rhsValue;
					local shouldProcess = true;
					local leftHandSide = expression4174.lhs;
					if (leftHandSide and leftHandSide.precedence or -100) <= (expression4174 and expression4174.precedence or -100) then
						shouldProcess = true;
						if expression4174.rhs.t ~= "multiplication" then
							shouldProcess = expression4174.rhs.t == "division";
						end;
					end;
					expressionFunction5(leftHandSide14, expressionValue4175, shouldProcess);
					decompiledCode = decompiledCode .. " * ";
					expressionFunction5 = expressionHandlerFunction;
					leftHandSide14 = expression4174.rhs;
					expressionValue4175 = rhsValue;
					shouldProcess = true;
					leftHandSide = expression4174.rhs;
					if (leftHandSide and leftHandSide.precedence or -100) <= (expression4174 and expression4174.precedence or -100) then
						shouldProcess = true;
						if expression4174.rhs.t ~= "multiplication" then
							shouldProcess = expression4174.rhs.t == "division";
						end;
					end;
					expressionFunction5(leftHandSide14, expressionValue4175, shouldProcess);
				end, 
				division = function(expression4181, v4182)
					local expressionFunction6 = expressionHandlerFunction;
					local leftHandSide16 = expression4181.lhs;
					local expressionValue4182 = v4182;
					local shouldProcess = true;
					local leftHandSide = expression4181.lhs;
					if (leftHandSide and leftHandSide.precedence or -100) <= (expression4181 and expression4181.precedence or -100) then
						shouldProcess = true;
						if expression4181.rhs.t ~= "multiplication" then
							shouldProcess = expression4181.rhs.t == "division";
						end;
					end;
					expressionFunction6(leftHandSide16, expressionValue4182, shouldProcess);
					decompiledCode = decompiledCode .. " / ";
					expressionFunction6 = expressionHandlerFunction;
					leftHandSide16 = expression4181.rhs;
					expressionValue4182 = v4182;
					shouldProcess = true;
					leftHandSide = expression4181.rhs;
					if (leftHandSide and leftHandSide.precedence or -100) <= (expression4181 and expression4181.precedence or -100) then
						shouldProcess = true;
						if expression4181.rhs.t ~= "multiplication" then
							shouldProcess = expression4181.rhs.t == "division";
						end;
					end;
					expressionFunction6(leftHandSide16, expressionValue4182, shouldProcess);
				end, 
				["floor division"] = function(binaryOperation, v4189)
					local expressionFunction7 = expressionHandlerFunction;
					local leftHandSide18 = binaryOperation.lhs;
					local expressionValue4189 = v4189;
					local leftHandSide = binaryOperation.lhs;
					expressionFunction7(leftHandSide18, expressionValue4189, (leftHandSide and leftHandSide.precedence or -100) > (binaryOperation and binaryOperation.precedence or -100));
					decompiledCode = decompiledCode .. " // ";
					expressionFunction7 = expressionHandlerFunction;
					leftHandSide18 = binaryOperation.rhs;
					expressionValue4189 = v4189;
					leftHandSide = binaryOperation.rhs;
					expressionFunction7(leftHandSide18, expressionValue4189, (leftHandSide and leftHandSide.precedence or -100) > (binaryOperation and binaryOperation.precedence or -100));
				end, 
				modulus = function(binaryOperation, v4195)
					local expressionFunction8 = expressionHandlerFunction;
					local leftHandSide20 = binaryOperation.lhs;
					local expressionValue4195 = v4195;
					local leftHandSide = binaryOperation.lhs;
					expressionFunction8(leftHandSide20, expressionValue4195, (leftHandSide and leftHandSide.precedence or -100) > (binaryOperation and binaryOperation.precedence or -100));
					decompiledCode = decompiledCode .. " % ";
					expressionFunction8 = expressionHandlerFunction;
					leftHandSide20 = binaryOperation.rhs;
					expressionValue4195 = v4195;
					leftHandSide = binaryOperation.rhs;
					expressionFunction8(leftHandSide20, expressionValue4195, (leftHandSide and leftHandSide.precedence or -100) > (binaryOperation and binaryOperation.precedence or -100));
				end, 
				negate = function(binaryOperation, v4201)
					decompiledCode = decompiledCode .. "-";
					local expressionFunction9 = expressionHandlerFunction;
					local rightHandSide9 = binaryOperation.rhs;
					local expressionValue4201 = v4201;
					local rightHandSide = binaryOperation.rhs;
					expressionFunction9(rightHandSide9, expressionValue4201, (rightHandSide and rightHandSide.precedence or -100) >= (binaryOperation and binaryOperation.precedence or -100));
				end, 
				["not"] = function(binaryOperation, v4207)
					decompiledCode = decompiledCode .. "not ";
					local expressionFunction10 = expressionHandlerFunction;
					local rightHandSide11 = binaryOperation.rhs;
					local expressionValue4207 = v4207;
					local rightHandSide = binaryOperation.rhs;
					expressionFunction10(rightHandSide11, expressionValue4207, (rightHandSide and rightHandSide.precedence or -100) > (binaryOperation and binaryOperation.precedence or -100));
				end, 
				length = function(binaryOperation, v4213)
					decompiledCode = decompiledCode .. "#";
					local expressionFunction11 = expressionHandlerFunction;
					local rightHandSide13 = binaryOperation.rhs;
					local expressionValue4213 = v4213;
					local rightHandSide = binaryOperation.rhs;
					expressionFunction11(rightHandSide13, expressionValue4213, (rightHandSide and rightHandSide.precedence or -100) >= (binaryOperation and binaryOperation.precedence or -100));
				end, 
				concatenation = function(expressionList, environment)
					for expressionIndex, expressionValue in ipairs(expressionList.exprs) do
						if expressionIndex > 1 then
							decompiledCode = decompiledCode .. "..";
						end;
						expressionHandlerFunction(expressionValue, environment, (expressionList and expressionList.precedence or -100) < (expressionValue and expressionValue.precedence or -100));
					end;
				end, 
				exponentiation = function(binaryOperation, v4223)
					local expressionFunction12 = expressionHandlerFunction;
					local leftHandSide22 = binaryOperation.lhs;
					local expressionValue4223 = v4223;
					local leftHandSide = binaryOperation.lhs;
					expressionFunction12(leftHandSide22, expressionValue4223, (leftHandSide and leftHandSide.precedence or -100) > (binaryOperation and binaryOperation.precedence or -100));
					decompiledCode = decompiledCode .. " ^ ";
					expressionFunction12 = expressionHandlerFunction;
					leftHandSide22 = binaryOperation.rhs;
					expressionValue4223 = v4223;
					leftHandSide = binaryOperation.rhs;
					expressionFunction12(leftHandSide22, expressionValue4223, (leftHandSide and leftHandSide.precedence or -100) > (binaryOperation and binaryOperation.precedence or -100));
				end, 
				["get table"] = function(expression4228, expression4229)
					local expressionTable = expression4228.table;
					while expressionTable.t == "name" do
						local overrideExpression = expressionTable.name.override_expr;
						if overrideExpression then
							expressionTable = overrideExpression;
						else
							break;
						end;
					end;
					local expressionTableAlias = expressionTable;
					expressionTable = expressionHandlerFunction;
					local expressionTableAliasAlias = expressionTableAlias;
					local expressionValue4229 = expression4229;
					local isPrecedenceValid = true;
					if expressionTableAlias.t ~= "new table" then
						isPrecedenceValid = (expressionTableAlias and expressionTableAlias.precedence or -100) >= 0;
					end;
					expressionTable(expressionTableAliasAlias, expressionValue4229, isPrecedenceValid);
					if expression4228.index.t == "constant" and expression4228.index.const.type == 3 and isValidIdentifier(expression4228.index.const.value) then
						decompiledCode = decompiledCode .. "." .. expression4228.index.const.value;
						return;
					else
						decompiledCode = decompiledCode .. "[";
						expressionHandlerFunction(expression4228.index, expression4229);
						decompiledCode = decompiledCode .. "]";
						return;
					end;
				end
			};
			useTonumberNan = settings.do_tonumber_nan;
			appendValueToString = function(dataValue, environment)
				local valueType = dataValue.type;
				if valueType == 0 then
					decompiledCode = decompiledCode .. "nil";
					return;
				elseif valueType == 1 then
					decompiledCode = decompiledCode .. (dataValue.value and "true" or "false");
					return;
				elseif valueType == 3 then
					local stringValue = dataValue.value;
					local cachedString = variableMappedLongStringConstants[stringValue];
					if cachedString then
						decompiledCode = decompiledCode .. cachedString;
						return;
					else
						totalOutputLength = totalOutputLength + #decompiledCode;
						table.insert(outputLines, decompiledCode);
						decompiledCode = "";
						decompiledCode = decompiledCode .. stringQuoteHandler(settings.string_quotes_behavior, stringValue);
						totalOutputLength = totalOutputLength + #decompiledCode;
						table.insert(outputLines, decompiledCode);
						decompiledCode = "";
						return;
					end;
				elseif valueType == 2 then
					decompiledCode = decompiledCode .. formatNumber(dataValue.value);
					return;
				elseif valueType == 7 then
					decompiledCode = decompiledCode .. "Vector3.new(" .. formatNumber(dataValue.value.X, true) .. ", " .. formatNumber(dataValue.value.Y, true) .. ", " .. formatNumber(dataValue.value.Z, true) .. ")";
					return;
				elseif valueType == 4 then
					expressionHandlerFunction(dataValue.value, environment);
					return;
				else
					error((("Unknown const type %*"):format(valueType)));
					return;
				end;
			end;
			local firstCharacter = string.sub(indentationString, 1, 1);
			expressionHandlerFunction = function(astExpressionType, argumentValue, isMultiline)
				local callbackFunction = expressionHandlers[astExpressionType.t];
				if callbackFunction then
					if isMultiline then
						local isSemicolonNeeded = false;
						if #outputLines > 1 then
							local isAllWhitespace = true;
							for charIndex = #decompiledCode, 1, -1 do
								if string.sub(decompiledCode, charIndex, charIndex) ~= firstCharacter then
									isAllWhitespace = false;
									break;
								end;
							end;
							if isAllWhitespace then
								local previousLine = outputLines[#outputLines];
								if previousLine then
									for charIndex = #previousLine, 1, -1 do
										local currentChar = string.sub(previousLine, charIndex, charIndex);
										if currentChar ~= firstCharacter then
											if currentChar == "\n" then
												isSemicolonNeeded = true;
												break;
											else
												break;
											end;
										end;
									end;
								end;
							end;
						end;
						if isSemicolonNeeded then
							decompiledCode = decompiledCode .. ";(";
						else
							decompiledCode = decompiledCode .. "(";
						end;
					end;
					local success, result = pcall(callbackFunction, astExpressionType, argumentValue);
					if not success then
						statementHandlers.comment(createCommentObject(": First try: " .. result .. "\n"), argumentValue);
					end;
					if isMultiline then
						decompiledCode = decompiledCode .. ")";
						return;
					end;
				else
					error((("Unknown AST expr type \"%*\""):format(astExpressionType.t)));
				end;
			end;
			processAstNodes = function(astNodes, indentation)
				for _, astNode in ipairs(astNodes) do
					local nodeHandler = statementHandlers[astNode.t];
					if nodeHandler then
						if astNode.t ~= "nothing" then
							decompiledCode = decompiledCode .. indentation;
							nodeHandler(astNode, indentation);
							decompiledCode = decompiledCode .. "\n";
							totalOutputLength = totalOutputLength + #decompiledCode;
							table.insert(outputLines, decompiledCode);
							decompiledCode = "";
						end;
					else
						error((("Unknown AST line type \"%*\""):format(astNode.t)));
					end;
				end;
			end;
			local tableBuffer = table.create(#astNodes);
			for _, astNode in ipairs(astNodes) do
				if astNode.t ~= "nothing" then
					if not astNode.t then
						print(astNode);
					end;
					table.insert(tableBuffer, astNode);
				end;
			end;
			processAstNodes(tableBuffer, "");
			local elapsedTime = clockFunction() - startTime;
			local tableIndex = 2;
			local function appendString(stringToAppend)
				local tableIndexCopy = tableIndex;
				totalOutputLength = totalOutputLength + #stringToAppend;
				table.insert(outputLines, tableIndexCopy, stringToAppend);
				tableIndex = tableIndex + 1;
			end;
			appendString((("-- Time taken: %* seconds\n"):format((string.format("%.6f", elapsedTime)))));
			if #noticesList > 0 then
				appendString("\n");
				local globalErrors = {};
				local globalWarnings = {};
				local globalInformation = {};
				local table1 = {};
				local table2 = {};
				local table3 = {};
				local errorMap = table1 --[[ copy: 146 -> 155 ]];
				local usageCounts = globalErrors --[[ copy: 143 -> 156 ]];
				local warningMap = table2 --[[ copy: 147 -> 157 ]];
				local usageList = globalWarnings --[[ copy: 144 -> 158 ]];
				local informationMap = table3 --[[ copy: 148 -> 159 ]];
				local contentList = globalInformation --[[ copy: 145 -> 160 ]];
				local function logNotice(noticeType, noticeContent)
					if noticeType == "error" then
						local errorEntry = errorMap[noticeContent];
						if errorEntry then
							errorEntry.uses = errorEntry.uses + 1;
							return;
						else
							local errorData = {
								uses = 1, 
								content = noticeContent
							};
							errorMap[noticeContent] = errorData;
							table.insert(usageCounts, errorData);
							return;
						end;
					elseif noticeType == "warning" then
						local existingUsage = warningMap[noticeContent];
						if existingUsage then
							existingUsage.uses = existingUsage.uses + 1;
							return;
						else
							local warningData = {
								uses = 1, 
								content = noticeContent
							};
							warningMap[noticeContent] = warningData;
							table.insert(usageList, warningData);
							return;
						end;
					elseif noticeType == "info" then
						local existingContent = informationMap[noticeContent];
						if existingContent then
							existingContent.uses = existingContent.uses + 1;
							return;
						else
							local informationData = {
								uses = 1, 
								content = noticeContent
							};
							informationMap[noticeContent] = informationData;
							table.insert(contentList, informationData);
							return;
						end;
					else
						logNotice("error", (("Unknown notice type \"%*\""):format(noticeType)));
						return;
					end;
				end;
				for _, dataItem in ipairs(noticesList) do
					logNotice(dataItem.type, dataItem.content);
				end;
				local function logNotices(commentText, noticePrefix, noticeList)
					if #noticeList > 0 then
						local noticeHeader = "-- " .. commentText .. "[" .. #noticeList .. "]:\n";
						local currentTableIndex = tableIndex;
						totalOutputLength = totalOutputLength + #noticeHeader;
						table.insert(outputLines, currentTableIndex, noticeHeader);
						tableIndex = tableIndex + 1;
						for noticeIndex, noticeData in ipairs(noticeList) do
							local noticeUses = noticeData.uses;
							local noticeText = noticeData.content;
							if string.match(noticeText, "\n") then
								if noticeUses == 1 then
									local singleUseNotice = "--[[ " .. noticeIndex .. ". " .. noticePrefix .. ":\n" .. noticeText .. "\n]]\n";
									local currentTableIndex = tableIndex;
									totalOutputLength = totalOutputLength + #singleUseNotice;
									table.insert(outputLines, currentTableIndex, singleUseNotice);
									tableIndex = tableIndex + 1;
								else
									local multiUseNotice = "--[[ " .. noticeIndex .. ". " .. noticePrefix .. " [x" .. noticeUses .. "]:n" .. noticeText .. "\n]]\n";
									local currentTableIndex = tableIndex;
									totalOutputLength = totalOutputLength + #multiUseNotice;
									table.insert(outputLines, currentTableIndex, multiUseNotice);
									tableIndex = tableIndex + 1;
								end;
							elseif noticeUses == 1 then
								local singleUseNotice = "---- " .. noticeIndex .. ". " .. noticePrefix .. ": " .. noticeText .. "\n";
								local currentTableIndex = tableIndex;
								totalOutputLength = totalOutputLength + #singleUseNotice;
								table.insert(outputLines, currentTableIndex, singleUseNotice);
								tableIndex = tableIndex + 1;
							else
								local multiUseNotice = "---- " .. noticeIndex .. ". " .. noticePrefix .. " [x" .. noticeUses .. "]: " .. noticeText .. "\n";
								local currentTableIndex = tableIndex;
								totalOutputLength = totalOutputLength + #multiUseNotice;
								table.insert(outputLines, currentTableIndex, multiUseNotice);
								tableIndex = tableIndex + 1;
							end;
						end;
					end;
				end;
				logNotices("Global Errors", prefixErrorValue, globalErrors);
				logNotices("Global Warnings", prefixWarning, globalWarnings);
				logNotices("Global Information", prefixInformation, globalInformation);
			end;
			appendString("\n");
			for _, stringIndex in ipairs(variableMappedLongStringConstantsOrder) do
				if settings.mark_reads_and_writes then
					appendString("-- " .. formatVariableUsage("Variable", longStringUsageCounts[stringIndex], 1, true) .. "\n");
				end;
				appendString("local " .. variableMappedLongStringConstants[stringIndex] .. " = ");
				appendString(stringQuoteHandler(settings.string_quotes_behavior, stringIndex));
				appendString("\n");
			end;
			benchmarkTimer:end_benchmark("Global AST To String");
			benchmarkTimer:start_benchmark("Global String Writing");
			local bufferObject = buffer.create(totalOutputLength);
			local bufferOffset = 0;
			for _, stringEntry in ipairs(outputLines) do
				buffer.copy(bufferObject, bufferOffset, buffer.fromstring(stringEntry));
				bufferOffset = bufferOffset + #stringEntry;
			end;
			while buffer.readu8(bufferObject, buffer.len(bufferObject) - 1) == 10 do
				local bufferLengthMinusOne = buffer.len(bufferObject) - 1;
				local newBuffer = buffer.create(bufferLengthMinusOne);
				buffer.copy(newBuffer, 0, bufferObject, 0, bufferLengthMinusOne);
				bufferObject = newBuffer;
			end;
			local stringBuffer = buffer.tostring(bufferObject);
			benchmarkTimer:end_benchmark("Global String Writing");
			benchmarkTimer:print_all_times();
			return stringBuffer;
		end;
	end;
	local function _(compressString, useXPCall)
		local function getBytecodeString(input)
			if type(input) == "string" then
				return compressString(input);
			else
				assert(getScriptBytecodeFunction, "getscriptbytecode was undefined");
				return compressString(getScriptBytecodeFunction(input));
			end;
		end;
		if useXPCall then
			return function(...)
				local success, errorMessage = xpcall(getBytecodeString, function(errorMessage_1)
					return tostring(errorMessage_1) .. "\n" .. debug.traceback(nil, 2);
				end, ...);
				if success then
					return errorMessage;
				else
					return prefixError .. ": After: " .. errorMessage;
				end;
			end;
		else
			return getBytecodeString;
		end;
	end;
	local function decompileFunction(bytecode)
		if type(bytecode) == "string" then
			return decompileBytecode(bytecode);
		else
			assert(getScriptBytecodeFunction, "getscriptbytecode was undefined");
			return decompileBytecode(getScriptBytecodeFunction(bytecode));
		end;
	end;
	local decompileFunctionCopy = decompileFunction --[[ copy: 105 -> 109 ]];
	local function xpcallDecompile(...)
		local success, result = xpcall(decompileFunctionCopy, function(errorMessage_2)
			return tostring(errorMessage_2) .. "\n" .. debug.traceback(nil, 2);
		end, ...);
		if success then
			return result;
		else
			return prefixError .. ": After: " .. result;
		end;
	end;
	decompileFunction = function(scriptBytecode)
		opcodeEncodingType = "vanilla";
		local vanillaDecompileResult = xpcallDecompile(scriptBytecode);
		local vanillaVersion = globalFailedInstructionsCount;
		opcodeEncodingType = "mul227";
		local mul227DecompileResult = xpcallDecompile(scriptBytecode);
		local mul227Version = globalFailedInstructionsCount;
		if mul227Version < vanillaVersion then
			return mul227DecompileResult;
		elseif vanillaVersion < mul227Version then
			return vanillaDecompileResult;
		elseif string.match(vanillaDecompileResult, "Unknown opcode") then
			return mul227DecompileResult;
		else
			return vanillaDecompileResult;
		end;
	end;
	local function disassembleFunction(bytecodeToDisassemble)
		if type(bytecodeToDisassemble) == "string" then
			return disassembleBytecode(bytecodeToDisassemble);
		else
			assert(getScriptBytecodeFunction, "getscriptbytecode was undefined");
			return disassembleBytecode(getScriptBytecodeFunction(bytecodeToDisassemble));
		end;
	end;
	local function xpcallDisassemble(...)
		local successDisassemble, resultDisassemble = xpcall(disassembleFunction, function(errorMessage_3)
			return tostring(errorMessage_3) .. "\n" .. debug.traceback(nil, 2);
		end, ...);
		if successDisassemble then
			return resultDisassemble;
		else
			return prefixError .. ": After: " .. resultDisassemble;
		end;
	end;
	return {
		decompile = decompileFunction, 
		disassemble = function(scriptBytecodeDisassemble)
			opcodeEncodingType = "vanilla";
			local vanillaDisassembleResult = xpcallDisassemble(scriptBytecodeDisassemble);
			local vanillaVersionDisassemble = globalFailedInstructionsCount;
			opcodeEncodingType = "mul227";
			local mul227DisassembleResult = xpcallDisassemble(scriptBytecodeDisassemble);
			local mul227VersionDisassemble = globalFailedInstructionsCount;
			if mul227VersionDisassemble < vanillaVersionDisassemble then
				return mul227DisassembleResult;
			elseif vanillaVersionDisassemble < mul227VersionDisassemble then
				return vanillaDisassembleResult;
			elseif #mul227DisassembleResult >= #vanillaDisassembleResult then
				return mul227DisassembleResult;
			else
				return vanillaDisassembleResult;
			end;
		end
	};
