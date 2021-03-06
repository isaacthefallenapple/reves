(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



// SEND REQUEST

var _Http_toTask = F3(function(router, toTask, request)
{
	return _Scheduler_binding(function(callback)
	{
		function done(response) {
			callback(toTask(request.expect.a(response)));
		}

		var xhr = new XMLHttpRequest();
		xhr.addEventListener('error', function() { done($elm$http$Http$NetworkError_); });
		xhr.addEventListener('timeout', function() { done($elm$http$Http$Timeout_); });
		xhr.addEventListener('load', function() { done(_Http_toResponse(request.expect.b, xhr)); });
		$elm$core$Maybe$isJust(request.tracker) && _Http_track(router, xhr, request.tracker.a);

		try {
			xhr.open(request.method, request.url, true);
		} catch (e) {
			return done($elm$http$Http$BadUrl_(request.url));
		}

		_Http_configureRequest(xhr, request);

		request.body.a && xhr.setRequestHeader('Content-Type', request.body.a);
		xhr.send(request.body.b);

		return function() { xhr.c = true; xhr.abort(); };
	});
});


// CONFIGURE

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.headers; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}
	xhr.timeout = request.timeout.a || 0;
	xhr.responseType = request.expect.d;
	xhr.withCredentials = request.allowCookiesFromOtherDomains;
}


// RESPONSES

function _Http_toResponse(toBody, xhr)
{
	return A2(
		200 <= xhr.status && xhr.status < 300 ? $elm$http$Http$GoodStatus_ : $elm$http$Http$BadStatus_,
		_Http_toMetadata(xhr),
		toBody(xhr.response)
	);
}


// METADATA

function _Http_toMetadata(xhr)
{
	return {
		url: xhr.responseURL,
		statusCode: xhr.status,
		statusText: xhr.statusText,
		headers: _Http_parseHeaders(xhr.getAllResponseHeaders())
	};
}


// HEADERS

function _Http_parseHeaders(rawHeaders)
{
	if (!rawHeaders)
	{
		return $elm$core$Dict$empty;
	}

	var headers = $elm$core$Dict$empty;
	var headerPairs = rawHeaders.split('\r\n');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf(': ');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3($elm$core$Dict$update, key, function(oldValue) {
				return $elm$core$Maybe$Just($elm$core$Maybe$isJust(oldValue)
					? value + ', ' + oldValue.a
					: value
				);
			}, headers);
		}
	}
	return headers;
}


// EXPECT

var _Http_expect = F3(function(type, toBody, toValue)
{
	return {
		$: 0,
		d: type,
		b: toBody,
		a: toValue
	};
});

var _Http_mapExpect = F2(function(func, expect)
{
	return {
		$: 0,
		d: expect.d,
		b: expect.b,
		a: function(x) { return func(expect.a(x)); }
	};
});

function _Http_toDataView(arrayBuffer)
{
	return new DataView(arrayBuffer);
}


// BODY and PARTS

var _Http_emptyBody = { $: 0 };
var _Http_pair = F2(function(a, b) { return { $: 0, a: a, b: b }; });

function _Http_toFormData(parts)
{
	for (var formData = new FormData(); parts.b; parts = parts.b) // WHILE_CONS
	{
		var part = parts.a;
		formData.append(part.a, part.b);
	}
	return formData;
}

var _Http_bytesToBlob = F2(function(mime, bytes)
{
	return new Blob([bytes], { type: mime });
});


// PROGRESS

function _Http_track(router, xhr, tracker)
{
	// TODO check out lengthComputable on loadstart event

	xhr.upload.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Sending({
			sent: event.loaded,
			size: event.total
		}))));
	});
	xhr.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Receiving({
			received: event.loaded,
			size: event.lengthComputable ? $elm$core$Maybe$Just(event.total) : $elm$core$Maybe$Nothing
		}))));
	});
}

function _Url_percentEncode(string)
{
	return encodeURIComponent(string);
}

function _Url_percentDecode(string)
{
	try
	{
		return $elm$core$Maybe$Just(decodeURIComponent(string));
	}
	catch (e)
	{
		return $elm$core$Maybe$Nothing;
	}
}


var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});



// DECODER

var _File_decoder = _Json_decodePrim(function(value) {
	// NOTE: checks if `File` exists in case this is run on node
	return (typeof File !== 'undefined' && value instanceof File)
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FILE', value);
});


// METADATA

function _File_name(file) { return file.name; }
function _File_mime(file) { return file.type; }
function _File_size(file) { return file.size; }

function _File_lastModified(file)
{
	return $elm$time$Time$millisToPosix(file.lastModified);
}


// DOWNLOAD

var _File_downloadNode;

function _File_getDownloadNode()
{
	return _File_downloadNode || (_File_downloadNode = document.createElement('a'));
}

var _File_download = F3(function(name, mime, content)
{
	return _Scheduler_binding(function(callback)
	{
		var blob = new Blob([content], {type: mime});

		// for IE10+
		if (navigator.msSaveOrOpenBlob)
		{
			navigator.msSaveOrOpenBlob(blob, name);
			return;
		}

		// for HTML5
		var node = _File_getDownloadNode();
		var objectUrl = URL.createObjectURL(blob);
		node.href = objectUrl;
		node.download = name;
		_File_click(node);
		URL.revokeObjectURL(objectUrl);
	});
});

function _File_downloadUrl(href)
{
	return _Scheduler_binding(function(callback)
	{
		var node = _File_getDownloadNode();
		node.href = href;
		node.download = '';
		node.origin === location.origin || (node.target = '_blank');
		_File_click(node);
	});
}


// IE COMPATIBILITY

function _File_makeBytesSafeForInternetExplorer(bytes)
{
	// only needed by IE10 and IE11 to fix https://github.com/elm/file/issues/10
	// all other browsers can just run `new Blob([bytes])` directly with no problem
	//
	return new Uint8Array(bytes.buffer, bytes.byteOffset, bytes.byteLength);
}

function _File_click(node)
{
	// only needed by IE10 and IE11 to fix https://github.com/elm/file/issues/11
	// all other browsers have MouseEvent and do not need this conditional stuff
	//
	if (typeof MouseEvent === 'function')
	{
		node.dispatchEvent(new MouseEvent('click'));
	}
	else
	{
		var event = document.createEvent('MouseEvents');
		event.initMouseEvent('click', true, true, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);
		document.body.appendChild(node);
		node.dispatchEvent(event);
		document.body.removeChild(node);
	}
}


// UPLOAD

var _File_node;

function _File_uploadOne(mimes)
{
	return _Scheduler_binding(function(callback)
	{
		_File_node = document.createElement('input');
		_File_node.type = 'file';
		_File_node.accept = A2($elm$core$String$join, ',', mimes);
		_File_node.addEventListener('change', function(event)
		{
			callback(_Scheduler_succeed(event.target.files[0]));
		});
		_File_click(_File_node);
	});
}

function _File_uploadOneOrMore(mimes)
{
	return _Scheduler_binding(function(callback)
	{
		_File_node = document.createElement('input');
		_File_node.type = 'file';
		_File_node.multiple = true;
		_File_node.accept = A2($elm$core$String$join, ',', mimes);
		_File_node.addEventListener('change', function(event)
		{
			var elmFiles = _List_fromArray(event.target.files);
			callback(_Scheduler_succeed(_Utils_Tuple2(elmFiles.a, elmFiles.b)));
		});
		_File_click(_File_node);
	});
}


// CONTENT

function _File_toString(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(reader.result));
		});
		reader.readAsText(blob);
		return function() { reader.abort(); };
	});
}

function _File_toBytes(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(new DataView(reader.result)));
		});
		reader.readAsArrayBuffer(blob);
		return function() { reader.abort(); };
	});
}

function _File_toUrl(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(reader.result));
		});
		reader.readAsDataURL(blob);
		return function() { reader.abort(); };
	});
}

var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $author$project$Main$LinkClicked = function (a) {
	return {$: 'LinkClicked', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $author$project$Main$UrlChanged = function (a) {
	return {$: 'UrlChanged', a: a};
};
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$application = _Browser_application;
var $author$project$Main$Character = function (a) {
	return {$: 'Character', a: a};
};
var $author$project$Main$Landing = function (a) {
	return {$: 'Landing', a: a};
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $author$project$Main$Abilities = function (a) {
	return {$: 'Abilities', a: a};
};
var $author$project$Main$AbilitiesMsg = function (a) {
	return {$: 'AbilitiesMsg', a: a};
};
var $author$project$Main$GotPlayAid = function (a) {
	return {$: 'GotPlayAid', a: a};
};
var $author$project$Main$PlayAid = F2(
	function (a, b) {
		return {$: 'PlayAid', a: a, b: b};
	});
var $author$project$Session$character = function (session) {
	if (session.$ === 'Character') {
		var val = session.a;
		return $elm$core$Maybe$Just(val.character);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Abilities$GotMetadata = function (a) {
	return {$: 'GotMetadata', a: a};
};
var $author$project$Status$Loading = {$: 'Loading'};
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$json$Json$Decode$decodeString = _Json_runOnString;
var $elm$http$Http$BadStatus_ = F2(
	function (a, b) {
		return {$: 'BadStatus_', a: a, b: b};
	});
var $elm$http$Http$BadUrl_ = function (a) {
	return {$: 'BadUrl_', a: a};
};
var $elm$http$Http$GoodStatus_ = F2(
	function (a, b) {
		return {$: 'GoodStatus_', a: a, b: b};
	});
var $elm$http$Http$NetworkError_ = {$: 'NetworkError_'};
var $elm$http$Http$Receiving = function (a) {
	return {$: 'Receiving', a: a};
};
var $elm$http$Http$Sending = function (a) {
	return {$: 'Sending', a: a};
};
var $elm$http$Http$Timeout_ = {$: 'Timeout_'};
var $elm$core$Maybe$isJust = function (maybe) {
	if (maybe.$ === 'Just') {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === 'RBNode_elm_builtin') {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === 'RBNode_elm_builtin') {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === 'RBNode_elm_builtin') {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (_v0.$ === 'Just') {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$http$Http$expectStringResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'',
			$elm$core$Basics$identity,
			A2($elm$core$Basics$composeR, toResult, toMsg));
	});
var $elm$core$Result$mapError = F2(
	function (f, result) {
		if (result.$ === 'Ok') {
			var v = result.a;
			return $elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return $elm$core$Result$Err(
				f(e));
		}
	});
var $elm$http$Http$BadBody = function (a) {
	return {$: 'BadBody', a: a};
};
var $elm$http$Http$BadStatus = function (a) {
	return {$: 'BadStatus', a: a};
};
var $elm$http$Http$BadUrl = function (a) {
	return {$: 'BadUrl', a: a};
};
var $elm$http$Http$NetworkError = {$: 'NetworkError'};
var $elm$http$Http$Timeout = {$: 'Timeout'};
var $elm$http$Http$resolve = F2(
	function (toResult, response) {
		switch (response.$) {
			case 'BadUrl_':
				var url = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadUrl(url));
			case 'Timeout_':
				return $elm$core$Result$Err($elm$http$Http$Timeout);
			case 'NetworkError_':
				return $elm$core$Result$Err($elm$http$Http$NetworkError);
			case 'BadStatus_':
				var metadata = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadStatus(metadata.statusCode));
			default:
				var body = response.b;
				return A2(
					$elm$core$Result$mapError,
					$elm$http$Http$BadBody,
					toResult(body));
		}
	});
var $elm$http$Http$expectJson = F2(
	function (toMsg, decoder) {
		return A2(
			$elm$http$Http$expectStringResponse,
			toMsg,
			$elm$http$Http$resolve(
				function (string) {
					return A2(
						$elm$core$Result$mapError,
						$elm$json$Json$Decode$errorToString,
						A2($elm$json$Json$Decode$decodeString, decoder, string));
				}));
	});
var $elm$http$Http$emptyBody = _Http_emptyBody;
var $elm$http$Http$Request = function (a) {
	return {$: 'Request', a: a};
};
var $elm$http$Http$State = F2(
	function (reqs, subs) {
		return {reqs: reqs, subs: subs};
	});
var $elm$http$Http$init = $elm$core$Task$succeed(
	A2($elm$http$Http$State, $elm$core$Dict$empty, _List_Nil));
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$http$Http$updateReqs = F3(
	function (router, cmds, reqs) {
		updateReqs:
		while (true) {
			if (!cmds.b) {
				return $elm$core$Task$succeed(reqs);
			} else {
				var cmd = cmds.a;
				var otherCmds = cmds.b;
				if (cmd.$ === 'Cancel') {
					var tracker = cmd.a;
					var _v2 = A2($elm$core$Dict$get, tracker, reqs);
					if (_v2.$ === 'Nothing') {
						var $temp$router = router,
							$temp$cmds = otherCmds,
							$temp$reqs = reqs;
						router = $temp$router;
						cmds = $temp$cmds;
						reqs = $temp$reqs;
						continue updateReqs;
					} else {
						var pid = _v2.a;
						return A2(
							$elm$core$Task$andThen,
							function (_v3) {
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A2($elm$core$Dict$remove, tracker, reqs));
							},
							$elm$core$Process$kill(pid));
					}
				} else {
					var req = cmd.a;
					return A2(
						$elm$core$Task$andThen,
						function (pid) {
							var _v4 = req.tracker;
							if (_v4.$ === 'Nothing') {
								return A3($elm$http$Http$updateReqs, router, otherCmds, reqs);
							} else {
								var tracker = _v4.a;
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A3($elm$core$Dict$insert, tracker, pid, reqs));
							}
						},
						$elm$core$Process$spawn(
							A3(
								_Http_toTask,
								router,
								$elm$core$Platform$sendToApp(router),
								req)));
				}
			}
		}
	});
var $elm$http$Http$onEffects = F4(
	function (router, cmds, subs, state) {
		return A2(
			$elm$core$Task$andThen,
			function (reqs) {
				return $elm$core$Task$succeed(
					A2($elm$http$Http$State, reqs, subs));
			},
			A3($elm$http$Http$updateReqs, router, cmds, state.reqs));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$http$Http$maybeSend = F4(
	function (router, desiredTracker, progress, _v0) {
		var actualTracker = _v0.a;
		var toMsg = _v0.b;
		return _Utils_eq(desiredTracker, actualTracker) ? $elm$core$Maybe$Just(
			A2(
				$elm$core$Platform$sendToApp,
				router,
				toMsg(progress))) : $elm$core$Maybe$Nothing;
	});
var $elm$http$Http$onSelfMsg = F3(
	function (router, _v0, state) {
		var tracker = _v0.a;
		var progress = _v0.b;
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$filterMap,
					A3($elm$http$Http$maybeSend, router, tracker, progress),
					state.subs)));
	});
var $elm$http$Http$Cancel = function (a) {
	return {$: 'Cancel', a: a};
};
var $elm$http$Http$cmdMap = F2(
	function (func, cmd) {
		if (cmd.$ === 'Cancel') {
			var tracker = cmd.a;
			return $elm$http$Http$Cancel(tracker);
		} else {
			var r = cmd.a;
			return $elm$http$Http$Request(
				{
					allowCookiesFromOtherDomains: r.allowCookiesFromOtherDomains,
					body: r.body,
					expect: A2(_Http_mapExpect, func, r.expect),
					headers: r.headers,
					method: r.method,
					timeout: r.timeout,
					tracker: r.tracker,
					url: r.url
				});
		}
	});
var $elm$http$Http$MySub = F2(
	function (a, b) {
		return {$: 'MySub', a: a, b: b};
	});
var $elm$http$Http$subMap = F2(
	function (func, _v0) {
		var tracker = _v0.a;
		var toMsg = _v0.b;
		return A2(
			$elm$http$Http$MySub,
			tracker,
			A2($elm$core$Basics$composeR, toMsg, func));
	});
_Platform_effectManagers['Http'] = _Platform_createManager($elm$http$Http$init, $elm$http$Http$onEffects, $elm$http$Http$onSelfMsg, $elm$http$Http$cmdMap, $elm$http$Http$subMap);
var $elm$http$Http$command = _Platform_leaf('Http');
var $elm$http$Http$subscription = _Platform_leaf('Http');
var $elm$http$Http$request = function (r) {
	return $elm$http$Http$command(
		$elm$http$Http$Request(
			{allowCookiesFromOtherDomains: false, body: r.body, expect: r.expect, headers: r.headers, method: r.method, timeout: r.timeout, tracker: r.tracker, url: r.url}));
};
var $elm$http$Http$get = function (r) {
	return $elm$http$Http$request(
		{body: $elm$http$Http$emptyBody, expect: r.expect, headers: _List_Nil, method: 'GET', timeout: $elm$core$Maybe$Nothing, tracker: $elm$core$Maybe$Nothing, url: r.url});
};
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$json$Json$Decode$keyValuePairs = _Json_decodeKeyValuePairs;
var $elm$json$Json$Decode$dict = function (decoder) {
	return A2(
		$elm$json$Json$Decode$map,
		$elm$core$Dict$fromList,
		$elm$json$Json$Decode$keyValuePairs(decoder));
};
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Abilities$metadataDecoder = $elm$json$Json$Decode$dict($elm$json$Json$Decode$string);
var $author$project$Abilities$location = '/reves/data/abilities/';
var $author$project$Abilities$metadataLocation = $author$project$Abilities$location + 'metadata.json';
var $author$project$Abilities$setSelected = F2(
	function (selected, abilities) {
		return _Utils_update(
			abilities,
			{
				selected: function () {
					var _v0 = _Utils_Tuple2(
						selected,
						$author$project$Session$character(abilities.session));
					if (_v0.a.$ === 'Just') {
						var s = _v0.a.a;
						return s;
					} else {
						if (_v0.b.$ === 'Just') {
							var _v1 = _v0.a;
							var c = _v0.b.a;
							return c._class;
						} else {
							var _v2 = _v0.a;
							var _v3 = _v0.b;
							return '';
						}
					}
				}()
			});
	});
var $author$project$Abilities$init = F2(
	function (session, maybeSelected) {
		return _Utils_Tuple2(
			A2(
				$author$project$Abilities$setSelected,
				maybeSelected,
				{chosen: $elm$core$Dict$empty, metadata: $author$project$Status$Loading, selected: '', session: session, tabs: $elm$core$Dict$empty}),
			$elm$core$Platform$Cmd$batch(
				_List_fromArray(
					[
						$elm$http$Http$get(
						{
							expect: A2($elm$http$Http$expectJson, $author$project$Abilities$GotMetadata, $author$project$Abilities$metadataDecoder),
							url: $author$project$Abilities$metadataLocation
						})
					])));
	});
var $author$project$PlayAids$location = '/reves/data/play-aids/';
var $author$project$PlayAids$topicToString = function (topic) {
	switch (topic.$) {
		case 'Weapons':
			return 'weapons';
		case 'Armor':
			return 'armour';
		case 'Skills':
			return 'skills';
		default:
			return 'domains';
	}
};
var $author$project$PlayAids$init = F3(
	function (toMsg, topic, maybeSelected) {
		return _Utils_Tuple2(
			{aids: $author$project$Status$Loading, selected: maybeSelected, topic: topic},
			$elm$http$Http$get(
				{
					expect: A2(
						$elm$http$Http$expectJson,
						toMsg,
						$elm$json$Json$Decode$dict($elm$json$Json$Decode$string)),
					url: $author$project$PlayAids$location + ($author$project$PlayAids$topicToString(topic) + '.json')
				}));
	});
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Abilities$toSession = function ($) {
	return $.session;
};
var $author$project$Main$toSession = function (model) {
	switch (model.$) {
		case 'PickClass':
			var session = model.a;
			return session;
		case 'PickAssignment':
			var session = model.a;
			return session;
		case 'Character':
			var session = model.a;
			return session;
		case 'DecodeErr':
			var session = model.a;
			return session;
		case 'Abilities':
			var abilities = model.a;
			return $author$project$Abilities$toSession(abilities);
		case 'Landing':
			var session = model.a;
			return session;
		default:
			var session = model.a;
			return session;
	}
};
var $elm$core$Platform$Cmd$map = _Platform_map;
var $author$project$Main$wrap = F3(
	function (toModel, toMsg, _v0) {
		var subModel = _v0.a;
		var subMsg = _v0.b;
		return _Utils_Tuple2(
			toModel(subModel),
			A2($elm$core$Platform$Cmd$map, toMsg, subMsg));
	});
var $author$project$Main$changeRoute = F2(
	function (route, model) {
		var navKey = $author$project$Main$toSession(model);
		var _v0 = _Utils_Tuple2(model, route);
		_v0$9:
		while (true) {
			switch (_v0.b.$) {
				case 'Abilities':
					switch (_v0.a.$) {
						case 'Abilities':
							var abilities = _v0.a.a;
							var selected = _v0.b.a;
							return _Utils_Tuple2(
								$author$project$Main$Abilities(
									A2($author$project$Abilities$setSelected, selected, abilities)),
								$elm$core$Platform$Cmd$none);
						case 'Character':
							var session = _v0.a.a;
							var selected = _v0.b.a;
							return A3(
								$author$project$Main$wrap,
								$author$project$Main$Abilities,
								$author$project$Main$AbilitiesMsg,
								A2($author$project$Abilities$init, session, selected));
						default:
							break _v0$9;
					}
				case 'Root':
					switch (_v0.a.$) {
						case 'Character':
							var session = _v0.a.a;
							var _v1 = _v0.b;
							var _v2 = $author$project$Session$character(session);
							if (_v2.$ === 'Just') {
								return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
							} else {
								return _Utils_Tuple2(
									$author$project$Main$Landing(session),
									$elm$core$Platform$Cmd$none);
							}
						case 'Abilities':
							var abilities = _v0.a.a;
							var _v3 = _v0.b;
							return _Utils_Tuple2(
								$author$project$Main$Character(
									$author$project$Abilities$toSession(abilities)),
								$elm$core$Platform$Cmd$none);
						case 'PickClass':
							var session = _v0.a.a;
							var _v4 = _v0.b;
							return _Utils_Tuple2(
								$author$project$Main$Landing(session),
								$elm$core$Platform$Cmd$none);
						case 'PickAssignment':
							var _v5 = _v0.a;
							var _v6 = _v0.b;
							return _Utils_Tuple2(
								$author$project$Main$Landing(navKey),
								$elm$core$Platform$Cmd$none);
						case 'PlayAid':
							var _v8 = _v0.a;
							var session = _v8.a;
							var _v9 = _v0.b;
							return _Utils_Tuple2(
								$author$project$Main$Character(session),
								$elm$core$Platform$Cmd$none);
						default:
							break _v0$9;
					}
				default:
					switch (_v0.a.$) {
						case 'Character':
							var session = _v0.a.a;
							var _v7 = _v0.b;
							var topic = _v7.a;
							var selected = _v7.b;
							return A3(
								$author$project$Main$wrap,
								$author$project$Main$PlayAid(session),
								$elm$core$Basics$identity,
								A3($author$project$PlayAids$init, $author$project$Main$GotPlayAid, topic, selected));
						case 'PlayAid':
							var _v10 = _v0.a;
							var session = _v10.a;
							var _v11 = _v0.b;
							var topic = _v11.a;
							var selected = _v11.b;
							return A3(
								$author$project$Main$wrap,
								$author$project$Main$PlayAid(session),
								$elm$core$Basics$identity,
								A3($author$project$PlayAids$init, $author$project$Main$GotPlayAid, topic, selected));
						default:
							break _v0$9;
					}
			}
		}
		return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
	});
var $author$project$Session$Character = function (a) {
	return {$: 'Character', a: a};
};
var $author$project$Session$NoCharacter = function (a) {
	return {$: 'NoCharacter', a: a};
};
var $author$project$Session$Saved = {$: 'Saved'};
var $author$project$Session$SavedLocally = {$: 'SavedLocally'};
var $author$project$Session$Unsaved = {$: 'Unsaved'};
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$fail = _Json_fail;
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $author$project$Session$changesDecoder = A2(
	$elm$json$Json$Decode$andThen,
	function (n) {
		switch (n) {
			case 0:
				return $elm$json$Json$Decode$succeed($author$project$Session$Unsaved);
			case 1:
				return $elm$json$Json$Decode$succeed($author$project$Session$SavedLocally);
			case 2:
				return $elm$json$Json$Decode$succeed($author$project$Session$Saved);
			default:
				return $elm$json$Json$Decode$fail(
					'Invalid changes value: ' + $elm$core$String$fromInt(n));
		}
	},
	$elm$json$Json$Decode$int);
var $author$project$Character$Stats = function (name) {
	return function (_class) {
		return function (assignment) {
			return function (skills) {
				return function (domains) {
					return function (skillKnacks) {
						return function (domainKnacks) {
							return function (equipment) {
								return function (refresh) {
									return function (abilities) {
										return function (bonds) {
											return function (fallout) {
												return function (resistances) {
													return function (notes) {
														return {abilities: abilities, assignment: assignment, bonds: bonds, _class: _class, domainKnacks: domainKnacks, domains: domains, equipment: equipment, fallout: fallout, name: name, notes: notes, refresh: refresh, resistances: resistances, skillKnacks: skillKnacks, skills: skills};
													};
												};
											};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var $author$project$Ability$Ability = F4(
	function (name, flavor, boons, text) {
		return {boons: boons, flavor: flavor, name: name, text: text};
	});
var $author$project$Boon$GainDomainKnack = function (a) {
	return {$: 'GainDomainKnack', a: a};
};
var $author$project$Boon$GainSkillKnack = function (a) {
	return {$: 'GainSkillKnack', a: a};
};
var $author$project$Boon$Domain$Criminal = {$: 'Criminal'};
var $author$project$Boon$Domain$Hegemony = {$: 'Hegemony'};
var $author$project$Boon$Domain$HighSociety = {$: 'HighSociety'};
var $author$project$Boon$Domain$LowSociety = {$: 'LowSociety'};
var $author$project$Boon$Domain$Science = {$: 'Science'};
var $author$project$Boon$Domain$Weirdness = {$: 'Weirdness'};
var $author$project$Boon$Domain$fromString = function (s) {
	switch (s) {
		case 'Criminal':
			return $elm$core$Maybe$Just($author$project$Boon$Domain$Criminal);
		case 'High Society':
			return $elm$core$Maybe$Just($author$project$Boon$Domain$HighSociety);
		case 'Low Society':
			return $elm$core$Maybe$Just($author$project$Boon$Domain$LowSociety);
		case 'Weirdness':
			return $elm$core$Maybe$Just($author$project$Boon$Domain$Weirdness);
		case 'Hegemony':
			return $elm$core$Maybe$Just($author$project$Boon$Domain$Hegemony);
		case 'Science':
			return $elm$core$Maybe$Just($author$project$Boon$Domain$Science);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Boon$Domain$domainDecoder = A2(
	$elm$json$Json$Decode$andThen,
	A2(
		$elm$core$Basics$composeR,
		$author$project$Boon$Domain$fromString,
		A2(
			$elm$core$Basics$composeR,
			$elm$core$Maybe$map($elm$json$Json$Decode$succeed),
			$elm$core$Maybe$withDefault(
				$elm$json$Json$Decode$fail('error')))),
	$elm$json$Json$Decode$string);
var $author$project$Boon$GainDomains = function (a) {
	return {$: 'GainDomains', a: a};
};
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$list = _Json_decodeList;
var $author$project$Boon$newDomainDecoder = A2(
	$elm$json$Json$Decode$map,
	$author$project$Boon$GainDomains,
	A2(
		$elm$json$Json$Decode$field,
		'domain',
		$elm$json$Json$Decode$list($author$project$Boon$Domain$domainDecoder)));
var $author$project$Boon$GainEquipment = function (a) {
	return {$: 'GainEquipment', a: a};
};
var $author$project$Boon$newEquipmentDecoder = A2(
	$elm$json$Json$Decode$map,
	$author$project$Boon$GainEquipment,
	A2(
		$elm$json$Json$Decode$field,
		'equipment',
		$elm$json$Json$Decode$list($elm$json$Json$Decode$string)));
var $author$project$Boon$GainRefresh = function (a) {
	return {$: 'GainRefresh', a: a};
};
var $author$project$Boon$newRefreshDecoder = A2(
	$elm$json$Json$Decode$map,
	$author$project$Boon$GainRefresh,
	A2(
		$elm$json$Json$Decode$field,
		'refresh',
		$elm$json$Json$Decode$list($elm$json$Json$Decode$string)));
var $author$project$Boon$GainSkills = function (a) {
	return {$: 'GainSkills', a: a};
};
var $author$project$Boon$Skill$Compel = {$: 'Compel'};
var $author$project$Boon$Skill$Deceive = {$: 'Deceive'};
var $author$project$Boon$Skill$Hack = {$: 'Hack'};
var $author$project$Boon$Skill$Investigate = {$: 'Investigate'};
var $author$project$Boon$Skill$Patch = {$: 'Patch'};
var $author$project$Boon$Skill$Resist = {$: 'Resist'};
var $author$project$Boon$Skill$Scramble = {$: 'Scramble'};
var $author$project$Boon$Skill$Scrap = {$: 'Scrap'};
var $author$project$Boon$Skill$Skulk = {$: 'Skulk'};
var $author$project$Boon$Skill$Steal = {$: 'Steal'};
var $author$project$Boon$Skill$fromString = function (s) {
	switch (s) {
		case 'Compel':
			return $elm$core$Maybe$Just($author$project$Boon$Skill$Compel);
		case 'Deceive':
			return $elm$core$Maybe$Just($author$project$Boon$Skill$Deceive);
		case 'Hack':
			return $elm$core$Maybe$Just($author$project$Boon$Skill$Hack);
		case 'Patch':
			return $elm$core$Maybe$Just($author$project$Boon$Skill$Patch);
		case 'Scramble':
			return $elm$core$Maybe$Just($author$project$Boon$Skill$Scramble);
		case 'Scrap':
			return $elm$core$Maybe$Just($author$project$Boon$Skill$Scrap);
		case 'Skulk':
			return $elm$core$Maybe$Just($author$project$Boon$Skill$Skulk);
		case 'Investigate':
			return $elm$core$Maybe$Just($author$project$Boon$Skill$Investigate);
		case 'Steal':
			return $elm$core$Maybe$Just($author$project$Boon$Skill$Steal);
		case 'Resist':
			return $elm$core$Maybe$Just($author$project$Boon$Skill$Resist);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Boon$Skill$skillDecoder = A2(
	$elm$json$Json$Decode$andThen,
	A2(
		$elm$core$Basics$composeR,
		$author$project$Boon$Skill$fromString,
		A2(
			$elm$core$Basics$composeR,
			$elm$core$Maybe$map($elm$json$Json$Decode$succeed),
			$elm$core$Maybe$withDefault(
				$elm$json$Json$Decode$fail('error')))),
	$elm$json$Json$Decode$string);
var $author$project$Boon$newSkillDecoder = A2(
	$elm$json$Json$Decode$map,
	$author$project$Boon$GainSkills,
	A2(
		$elm$json$Json$Decode$field,
		'skill',
		$elm$json$Json$Decode$list($author$project$Boon$Skill$skillDecoder)));
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $author$project$Boon$GainResistance = F2(
	function (a, b) {
		return {$: 'GainResistance', a: a, b: b};
	});
var $author$project$Boon$Resistance$Armor = {$: 'Armor'};
var $author$project$Boon$Resistance$Body = {$: 'Body'};
var $author$project$Boon$Resistance$Reputation = {$: 'Reputation'};
var $author$project$Boon$Resistance$Resolve = {$: 'Resolve'};
var $author$project$Boon$Resistance$Resources = {$: 'Resources'};
var $author$project$Boon$Resistance$Shadow = {$: 'Shadow'};
var $author$project$Boon$Resistance$fromString = function (s) {
	switch (s) {
		case 'Body':
			return $elm$core$Maybe$Just($author$project$Boon$Resistance$Body);
		case 'Resolve':
			return $elm$core$Maybe$Just($author$project$Boon$Resistance$Resolve);
		case 'Resources':
			return $elm$core$Maybe$Just($author$project$Boon$Resistance$Resources);
		case 'Shadow':
			return $elm$core$Maybe$Just($author$project$Boon$Resistance$Shadow);
		case 'Reputation':
			return $elm$core$Maybe$Just($author$project$Boon$Resistance$Reputation);
		case 'Armour':
			return $elm$core$Maybe$Just($author$project$Boon$Resistance$Armor);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Boon$Resistance$resistanceDecoder = A2(
	$elm$json$Json$Decode$andThen,
	A2(
		$elm$core$Basics$composeR,
		$author$project$Boon$Resistance$fromString,
		A2(
			$elm$core$Basics$composeR,
			$elm$core$Maybe$map($elm$json$Json$Decode$succeed),
			$elm$core$Maybe$withDefault(
				$elm$json$Json$Decode$fail('error')))),
	$elm$json$Json$Decode$string);
var $author$project$Boon$resistanceUpDecoder = A3(
	$elm$json$Json$Decode$map2,
	$author$project$Boon$GainResistance,
	A2($elm$json$Json$Decode$field, 'resistance', $author$project$Boon$Resistance$resistanceDecoder),
	A2($elm$json$Json$Decode$field, 'bonus', $elm$json$Json$Decode$int));
var $author$project$Boon$decoder = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[
			$author$project$Boon$resistanceUpDecoder,
			$author$project$Boon$newSkillDecoder,
			$author$project$Boon$newDomainDecoder,
			$author$project$Boon$newEquipmentDecoder,
			$author$project$Boon$newRefreshDecoder,
			$elm$json$Json$Decode$oneOf(
			_List_fromArray(
				[
					A2($elm$json$Json$Decode$map, $author$project$Boon$GainSkillKnack, $author$project$Boon$Skill$skillDecoder),
					A2($elm$json$Json$Decode$map, $author$project$Boon$GainDomainKnack, $author$project$Boon$Domain$domainDecoder)
				]))
		]));
var $elm$json$Json$Decode$map4 = _Json_map4;
var $elm$json$Json$Decode$null = _Json_decodeNull;
var $elm$json$Json$Decode$nullable = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				$elm$json$Json$Decode$null($elm$core$Maybe$Nothing),
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder)
			]));
};
var $author$project$Ability$decoder = A5(
	$elm$json$Json$Decode$map4,
	$author$project$Ability$Ability,
	A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$field,
		'flavor',
		$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string)),
	A2(
		$elm$json$Json$Decode$field,
		'boons',
		$elm$json$Json$Decode$list($author$project$Boon$decoder)),
	A2($elm$json$Json$Decode$field, 'text', $elm$json$Json$Decode$string));
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $author$project$TypeDict$Json$Decode$decodeKVPair = F2(
	function (keyDecoder, valDecoder) {
		return A3(
			$elm$json$Json$Decode$map2,
			$elm$core$Tuple$pair,
			A2($elm$json$Json$Decode$field, 'key', keyDecoder),
			A2($elm$json$Json$Decode$field, 'val', valDecoder));
	});
var $author$project$TypeDict$Dict = function (a) {
	return {$: 'Dict', a: a};
};
var $author$project$TypeDict$fromList = F2(
	function (hasher, list) {
		return $author$project$TypeDict$Dict(
			{
				hasher: hasher,
				inner: $elm$core$Dict$fromList(
					A2(
						$elm$core$List$map,
						function (_v0) {
							var k = _v0.a;
							var v = _v0.b;
							return _Utils_Tuple2(
								hasher(k),
								_Utils_Tuple2(k, v));
						},
						list))
			});
	});
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $author$project$TypeDict$Json$Decode$decoder = F3(
	function (hasher, keyDecoder, valDecoder) {
		return A2(
			$elm$json$Json$Decode$map,
			A2(
				$elm$core$Basics$composeR,
				$elm$core$Dict$toList,
				A2(
					$elm$core$Basics$composeR,
					$elm$core$List$map($elm$core$Tuple$second),
					$author$project$TypeDict$fromList(hasher))),
			$elm$json$Json$Decode$dict(
				A2($author$project$TypeDict$Json$Decode$decodeKVPair, keyDecoder, valDecoder)));
	});
var $author$project$Boon$Domain$toString = function (domain) {
	switch (domain.$) {
		case 'Criminal':
			return 'Criminal';
		case 'HighSociety':
			return 'High Society';
		case 'LowSociety':
			return 'Low Society';
		case 'Weirdness':
			return 'Weirdness';
		case 'Hegemony':
			return 'Hegemony';
		default:
			return 'Science';
	}
};
var $author$project$Boon$Domain$decoder = A3($author$project$TypeDict$Json$Decode$decoder, $author$project$Boon$Domain$toString, $author$project$Boon$Domain$domainDecoder, $elm$json$Json$Decode$bool);
var $author$project$Boon$Resistance$toString = function (resistance) {
	switch (resistance.$) {
		case 'Body':
			return 'Body';
		case 'Resolve':
			return 'Resolve';
		case 'Resources':
			return 'Resources';
		case 'Shadow':
			return 'Shadow';
		case 'Reputation':
			return 'Reputation';
		default:
			return 'Armour';
	}
};
var $author$project$Boon$Resistance$decoder = A3($author$project$TypeDict$Json$Decode$decoder, $author$project$Boon$Resistance$toString, $author$project$Boon$Resistance$resistanceDecoder, $elm$json$Json$Decode$int);
var $author$project$Boon$Skill$toString = function (skill) {
	switch (skill.$) {
		case 'Compel':
			return 'Compel';
		case 'Deceive':
			return 'Deceive';
		case 'Hack':
			return 'Hack';
		case 'Patch':
			return 'Patch';
		case 'Scramble':
			return 'Scramble';
		case 'Scrap':
			return 'Scrap';
		case 'Skulk':
			return 'Skulk';
		case 'Investigate':
			return 'Investigate';
		case 'Steal':
			return 'Steal';
		default:
			return 'Resist';
	}
};
var $author$project$Boon$Skill$decoder = A3($author$project$TypeDict$Json$Decode$decoder, $author$project$Boon$Skill$toString, $author$project$Boon$Skill$skillDecoder, $elm$json$Json$Decode$bool);
var $elm$json$Json$Decode$array = _Json_decodeArray;
var $author$project$Boon$Knack$domainsDecoder = A3(
	$author$project$TypeDict$Json$Decode$decoder,
	$author$project$Boon$Domain$toString,
	$author$project$Boon$Domain$domainDecoder,
	$elm$json$Json$Decode$array($elm$json$Json$Decode$string));
var $author$project$Boon$Domain$new = A2(
	$author$project$TypeDict$fromList,
	$author$project$Boon$Domain$toString,
	A2(
		$elm$core$List$map,
		function (d) {
			return _Utils_Tuple2(d, false);
		},
		_List_fromArray(
			[$author$project$Boon$Domain$Criminal, $author$project$Boon$Domain$HighSociety, $author$project$Boon$Domain$LowSociety, $author$project$Boon$Domain$Weirdness, $author$project$Boon$Domain$Hegemony, $author$project$Boon$Domain$Science])));
var $author$project$Boon$Resistance$new = A2(
	$author$project$TypeDict$fromList,
	$author$project$Boon$Resistance$toString,
	A2(
		$elm$core$List$map,
		function (r) {
			return _Utils_Tuple2(r, 0);
		},
		_List_fromArray(
			[$author$project$Boon$Resistance$Body, $author$project$Boon$Resistance$Resolve, $author$project$Boon$Resistance$Resources, $author$project$Boon$Resistance$Shadow, $author$project$Boon$Resistance$Reputation, $author$project$Boon$Resistance$Armor])));
var $author$project$Boon$Skill$new = A2(
	$author$project$TypeDict$fromList,
	$author$project$Boon$Skill$toString,
	A2(
		$elm$core$List$map,
		function (s) {
			return _Utils_Tuple2(s, false);
		},
		_List_fromArray(
			[$author$project$Boon$Skill$Compel, $author$project$Boon$Skill$Deceive, $author$project$Boon$Skill$Hack, $author$project$Boon$Skill$Patch, $author$project$Boon$Skill$Scramble, $author$project$Boon$Skill$Scrap, $author$project$Boon$Skill$Skulk, $author$project$Boon$Skill$Investigate, $author$project$Boon$Skill$Steal, $author$project$Boon$Skill$Resist])));
var $author$project$TypeDict$empty = function (hasher) {
	return $author$project$TypeDict$Dict(
		{hasher: hasher, inner: $elm$core$Dict$empty});
};
var $author$project$Boon$Knack$newDomains = $author$project$TypeDict$empty($author$project$Boon$Domain$toString);
var $author$project$Boon$Knack$newSkills = $author$project$TypeDict$empty($author$project$Boon$Skill$toString);
var $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom = $elm$json$Json$Decode$map2($elm$core$Basics$apR);
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optionalDecoder = F3(
	function (pathDecoder, valDecoder, fallback) {
		var nullOr = function (decoder) {
			return $elm$json$Json$Decode$oneOf(
				_List_fromArray(
					[
						decoder,
						$elm$json$Json$Decode$null(fallback)
					]));
		};
		var handleResult = function (input) {
			var _v0 = A2($elm$json$Json$Decode$decodeValue, pathDecoder, input);
			if (_v0.$ === 'Ok') {
				var rawValue = _v0.a;
				var _v1 = A2(
					$elm$json$Json$Decode$decodeValue,
					nullOr(valDecoder),
					rawValue);
				if (_v1.$ === 'Ok') {
					var finalResult = _v1.a;
					return $elm$json$Json$Decode$succeed(finalResult);
				} else {
					var finalErr = _v1.a;
					return $elm$json$Json$Decode$fail(
						$elm$json$Json$Decode$errorToString(finalErr));
				}
			} else {
				return $elm$json$Json$Decode$succeed(fallback);
			}
		};
		return A2($elm$json$Json$Decode$andThen, handleResult, $elm$json$Json$Decode$value);
	});
var $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional = F4(
	function (key, valDecoder, fallback, decoder) {
		return A2(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom,
			A3(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optionalDecoder,
				A2($elm$json$Json$Decode$field, key, $elm$json$Json$Decode$value),
				valDecoder,
				fallback),
			decoder);
	});
var $author$project$Boon$Knack$skillsDecoder = A3(
	$author$project$TypeDict$Json$Decode$decoder,
	$author$project$Boon$Skill$toString,
	$author$project$Boon$Skill$skillDecoder,
	$elm$json$Json$Decode$array($elm$json$Json$Decode$string));
var $author$project$Character$decoder = A4(
	$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
	'notes',
	$elm$json$Json$Decode$string,
	'',
	A4(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
		'resistances',
		$author$project$Boon$Resistance$decoder,
		$author$project$Boon$Resistance$new,
		A4(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
			'fallout',
			$elm$json$Json$Decode$string,
			'',
			A4(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
				'bonds',
				$elm$json$Json$Decode$string,
				'',
				A4(
					$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
					'abilities',
					$elm$json$Json$Decode$dict($author$project$Ability$decoder),
					$elm$core$Dict$empty,
					A4(
						$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
						'refresh',
						$elm$json$Json$Decode$string,
						'',
						A4(
							$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
							'equipment',
							$elm$json$Json$Decode$string,
							'',
							A4(
								$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
								'domainKnacks',
								$author$project$Boon$Knack$domainsDecoder,
								$author$project$Boon$Knack$newDomains,
								A4(
									$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
									'skillKnacks',
									$author$project$Boon$Knack$skillsDecoder,
									$author$project$Boon$Knack$newSkills,
									A4(
										$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
										'domains',
										$author$project$Boon$Domain$decoder,
										$author$project$Boon$Domain$new,
										A4(
											$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
											'skills',
											$author$project$Boon$Skill$decoder,
											$author$project$Boon$Skill$new,
											A4(
												$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
												'assignment',
												$elm$json$Json$Decode$string,
												'',
												A4(
													$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
													'class',
													$elm$json$Json$Decode$string,
													'',
													A4(
														$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
														'name',
														$elm$json$Json$Decode$string,
														'',
														$elm$json$Json$Decode$succeed($author$project$Character$Stats)))))))))))))));
var $author$project$Session$decodeCharacterSession = function (key) {
	return A3(
		$elm$json$Json$Decode$map2,
		F2(
			function (_char, cs) {
				return {changes: cs, character: _char, navKey: key};
			}),
		A2($elm$json$Json$Decode$field, 'character', $author$project$Character$decoder),
		A2($elm$json$Json$Decode$field, 'changes', $author$project$Session$changesDecoder));
};
var $author$project$Session$decoder = function (key) {
	return A2(
		$elm$json$Json$Decode$map,
		$elm$core$Maybe$withDefault(
			$author$project$Session$NoCharacter(key)),
		$elm$json$Json$Decode$nullable(
			A2(
				$elm$json$Json$Decode$map,
				$author$project$Session$Character,
				$author$project$Session$decodeCharacterSession(key))));
};
var $author$project$Session$changes = function (session) {
	if (session.$ === 'Character') {
		var val = session.a;
		return val.changes;
	} else {
		return $author$project$Session$Saved;
	}
};
var $author$project$Session$isSaved = A2(
	$elm$core$Basics$composeR,
	$author$project$Session$changes,
	$elm$core$Basics$eq($author$project$Session$Saved));
var $elm$core$Tuple$mapSecond = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			x,
			func(y));
	});
var $author$project$Session$new = function (key) {
	return $author$project$Session$NoCharacter(key);
};
var $author$project$Route$Root = {$: 'Root'};
var $elm$url$Url$Parser$State = F5(
	function (visited, unvisited, params, frag, value) {
		return {frag: frag, params: params, unvisited: unvisited, value: value, visited: visited};
	});
var $elm$url$Url$Parser$getFirstMatch = function (states) {
	getFirstMatch:
	while (true) {
		if (!states.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			var state = states.a;
			var rest = states.b;
			var _v1 = state.unvisited;
			if (!_v1.b) {
				return $elm$core$Maybe$Just(state.value);
			} else {
				if ((_v1.a === '') && (!_v1.b.b)) {
					return $elm$core$Maybe$Just(state.value);
				} else {
					var $temp$states = rest;
					states = $temp$states;
					continue getFirstMatch;
				}
			}
		}
	}
};
var $elm$url$Url$Parser$removeFinalEmpty = function (segments) {
	if (!segments.b) {
		return _List_Nil;
	} else {
		if ((segments.a === '') && (!segments.b.b)) {
			return _List_Nil;
		} else {
			var segment = segments.a;
			var rest = segments.b;
			return A2(
				$elm$core$List$cons,
				segment,
				$elm$url$Url$Parser$removeFinalEmpty(rest));
		}
	}
};
var $elm$url$Url$Parser$preparePath = function (path) {
	var _v0 = A2($elm$core$String$split, '/', path);
	if (_v0.b && (_v0.a === '')) {
		var segments = _v0.b;
		return $elm$url$Url$Parser$removeFinalEmpty(segments);
	} else {
		var segments = _v0;
		return $elm$url$Url$Parser$removeFinalEmpty(segments);
	}
};
var $elm$url$Url$Parser$addToParametersHelp = F2(
	function (value, maybeList) {
		if (maybeList.$ === 'Nothing') {
			return $elm$core$Maybe$Just(
				_List_fromArray(
					[value]));
		} else {
			var list = maybeList.a;
			return $elm$core$Maybe$Just(
				A2($elm$core$List$cons, value, list));
		}
	});
var $elm$url$Url$percentDecode = _Url_percentDecode;
var $elm$url$Url$Parser$addParam = F2(
	function (segment, dict) {
		var _v0 = A2($elm$core$String$split, '=', segment);
		if ((_v0.b && _v0.b.b) && (!_v0.b.b.b)) {
			var rawKey = _v0.a;
			var _v1 = _v0.b;
			var rawValue = _v1.a;
			var _v2 = $elm$url$Url$percentDecode(rawKey);
			if (_v2.$ === 'Nothing') {
				return dict;
			} else {
				var key = _v2.a;
				var _v3 = $elm$url$Url$percentDecode(rawValue);
				if (_v3.$ === 'Nothing') {
					return dict;
				} else {
					var value = _v3.a;
					return A3(
						$elm$core$Dict$update,
						key,
						$elm$url$Url$Parser$addToParametersHelp(value),
						dict);
				}
			}
		} else {
			return dict;
		}
	});
var $elm$url$Url$Parser$prepareQuery = function (maybeQuery) {
	if (maybeQuery.$ === 'Nothing') {
		return $elm$core$Dict$empty;
	} else {
		var qry = maybeQuery.a;
		return A3(
			$elm$core$List$foldr,
			$elm$url$Url$Parser$addParam,
			$elm$core$Dict$empty,
			A2($elm$core$String$split, '&', qry));
	}
};
var $elm$url$Url$Parser$parse = F2(
	function (_v0, url) {
		var parser = _v0.a;
		return $elm$url$Url$Parser$getFirstMatch(
			parser(
				A5(
					$elm$url$Url$Parser$State,
					_List_Nil,
					$elm$url$Url$Parser$preparePath(url.path),
					$elm$url$Url$Parser$prepareQuery(url.query),
					url.fragment,
					$elm$core$Basics$identity)));
	});
var $author$project$Route$Abilities = function (a) {
	return {$: 'Abilities', a: a};
};
var $author$project$Route$PlayAid = F2(
	function (a, b) {
		return {$: 'PlayAid', a: a, b: b};
	});
var $elm$url$Url$Parser$Parser = function (a) {
	return {$: 'Parser', a: a};
};
var $elm$url$Url$Parser$fragment = function (toFrag) {
	return $elm$url$Url$Parser$Parser(
		function (_v0) {
			var visited = _v0.visited;
			var unvisited = _v0.unvisited;
			var params = _v0.params;
			var frag = _v0.frag;
			var value = _v0.value;
			return _List_fromArray(
				[
					A5(
					$elm$url$Url$Parser$State,
					visited,
					unvisited,
					params,
					frag,
					value(
						toFrag(frag)))
				]);
		});
};
var $elm$url$Url$Parser$mapState = F2(
	function (func, _v0) {
		var visited = _v0.visited;
		var unvisited = _v0.unvisited;
		var params = _v0.params;
		var frag = _v0.frag;
		var value = _v0.value;
		return A5(
			$elm$url$Url$Parser$State,
			visited,
			unvisited,
			params,
			frag,
			func(value));
	});
var $elm$url$Url$Parser$map = F2(
	function (subValue, _v0) {
		var parseArg = _v0.a;
		return $elm$url$Url$Parser$Parser(
			function (_v1) {
				var visited = _v1.visited;
				var unvisited = _v1.unvisited;
				var params = _v1.params;
				var frag = _v1.frag;
				var value = _v1.value;
				return A2(
					$elm$core$List$map,
					$elm$url$Url$Parser$mapState(value),
					parseArg(
						A5($elm$url$Url$Parser$State, visited, unvisited, params, frag, subValue)));
			});
	});
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $elm$url$Url$Parser$oneOf = function (parsers) {
	return $elm$url$Url$Parser$Parser(
		function (state) {
			return A2(
				$elm$core$List$concatMap,
				function (_v0) {
					var parser = _v0.a;
					return parser(state);
				},
				parsers);
		});
};
var $elm$url$Url$Parser$custom = F2(
	function (tipe, stringToSomething) {
		return $elm$url$Url$Parser$Parser(
			function (_v0) {
				var visited = _v0.visited;
				var unvisited = _v0.unvisited;
				var params = _v0.params;
				var frag = _v0.frag;
				var value = _v0.value;
				if (!unvisited.b) {
					return _List_Nil;
				} else {
					var next = unvisited.a;
					var rest = unvisited.b;
					var _v2 = stringToSomething(next);
					if (_v2.$ === 'Just') {
						var nextValue = _v2.a;
						return _List_fromArray(
							[
								A5(
								$elm$url$Url$Parser$State,
								A2($elm$core$List$cons, next, visited),
								rest,
								params,
								frag,
								value(nextValue))
							]);
					} else {
						return _List_Nil;
					}
				}
			});
	});
var $author$project$PlayAids$Armor = {$: 'Armor'};
var $author$project$PlayAids$Domains = {$: 'Domains'};
var $author$project$PlayAids$Skills = {$: 'Skills'};
var $author$project$PlayAids$Weapons = {$: 'Weapons'};
var $author$project$PlayAids$topicFromString = function (topic) {
	switch (topic) {
		case 'weapons':
			return $elm$core$Maybe$Just($author$project$PlayAids$Weapons);
		case 'armour':
			return $elm$core$Maybe$Just($author$project$PlayAids$Armor);
		case 'skills':
			return $elm$core$Maybe$Just($author$project$PlayAids$Skills);
		case 'domains':
			return $elm$core$Maybe$Just($author$project$PlayAids$Domains);
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Route$playAid = A2($elm$url$Url$Parser$custom, 'TOPIC', $author$project$PlayAids$topicFromString);
var $elm$url$Url$Parser$s = function (str) {
	return $elm$url$Url$Parser$Parser(
		function (_v0) {
			var visited = _v0.visited;
			var unvisited = _v0.unvisited;
			var params = _v0.params;
			var frag = _v0.frag;
			var value = _v0.value;
			if (!unvisited.b) {
				return _List_Nil;
			} else {
				var next = unvisited.a;
				var rest = unvisited.b;
				return _Utils_eq(next, str) ? _List_fromArray(
					[
						A5(
						$elm$url$Url$Parser$State,
						A2($elm$core$List$cons, next, visited),
						rest,
						params,
						frag,
						value)
					]) : _List_Nil;
			}
		});
};
var $elm$url$Url$Parser$slash = F2(
	function (_v0, _v1) {
		var parseBefore = _v0.a;
		var parseAfter = _v1.a;
		return $elm$url$Url$Parser$Parser(
			function (state) {
				return A2(
					$elm$core$List$concatMap,
					parseAfter,
					parseBefore(state));
			});
	});
var $elm$url$Url$Parser$top = $elm$url$Url$Parser$Parser(
	function (state) {
		return _List_fromArray(
			[state]);
	});
var $author$project$Route$parser = A2(
	$elm$url$Url$Parser$slash,
	$elm$url$Url$Parser$s('reves'),
	$elm$url$Url$Parser$oneOf(
		_List_fromArray(
			[
				A2($elm$url$Url$Parser$map, $author$project$Route$Root, $elm$url$Url$Parser$top),
				A2(
				$elm$url$Url$Parser$map,
				$author$project$Route$Abilities,
				A2(
					$elm$url$Url$Parser$slash,
					$elm$url$Url$Parser$s('abilities'),
					$elm$url$Url$Parser$fragment($elm$core$Basics$identity))),
				A2(
				$elm$url$Url$Parser$map,
				$author$project$Route$PlayAid,
				A2(
					$elm$url$Url$Parser$slash,
					$elm$url$Url$Parser$s('play-aid'),
					A2(
						$elm$url$Url$Parser$slash,
						$author$project$Route$playAid,
						$elm$url$Url$Parser$fragment($elm$core$Basics$identity))))
			])));
var $author$project$Route$parse = function (url) {
	return A2(
		$elm$core$Maybe$withDefault,
		$author$project$Route$Root,
		A2($elm$url$Url$Parser$parse, $author$project$Route$parser, url));
};
var $elm$core$List$singleton = function (value) {
	return _List_fromArray(
		[value]);
};
var $elm$json$Json$Encode$null = _Json_encodeNull;
var $author$project$Ports$updatedCharacter = _Platform_outgoingPort(
	'updatedCharacter',
	function ($) {
		return $elm$json$Json$Encode$null;
	});
var $elm$core$Result$withDefault = F2(
	function (def, result) {
		if (result.$ === 'Ok') {
			var a = result.a;
			return a;
		} else {
			return def;
		}
	});
var $author$project$Main$init = F3(
	function (flags, url, navKey) {
		var model = function () {
			if (flags.$ === 'Nothing') {
				return $author$project$Main$Landing(
					$author$project$Session$new(navKey));
			} else {
				var json = flags.a;
				return $author$project$Main$Character(
					A2(
						$elm$core$Result$withDefault,
						$author$project$Session$new(navKey),
						A2(
							$elm$json$Json$Decode$decodeString,
							$author$project$Session$decoder(navKey),
							json)));
			}
		}();
		var cmd = $author$project$Session$isSaved(
			$author$project$Main$toSession(model)) ? $elm$core$Platform$Cmd$none : $author$project$Ports$updatedCharacter(_Utils_Tuple0);
		return A2(
			$elm$core$Tuple$mapSecond,
			A2(
				$elm$core$Basics$composeR,
				$elm$core$List$singleton,
				A2(
					$elm$core$Basics$composeR,
					$elm$core$List$cons(cmd),
					$elm$core$Platform$Cmd$batch)),
			A2(
				$author$project$Main$changeRoute,
				$author$project$Route$parse(url),
				model));
	});
var $author$project$Main$SavedChanges = {$: 'SavedChanges'};
var $author$project$Ports$confirmLocalStorage = _Platform_incomingPort(
	'confirmLocalStorage',
	$elm$json$Json$Decode$null(_Utils_Tuple0));
var $author$project$Main$subscriptions = function (_v0) {
	return $author$project$Ports$confirmLocalStorage(
		function (_v1) {
			return $author$project$Main$SavedChanges;
		});
};
var $author$project$Main$DecodeErr = F2(
	function (a, b) {
		return {$: 'DecodeErr', a: a, b: b};
	});
var $author$project$Main$FileLoaded = function (a) {
	return {$: 'FileLoaded', a: a};
};
var $author$project$Main$PickAssignment = F2(
	function (a, b) {
		return {$: 'PickAssignment', a: a, b: b};
	});
var $author$project$Main$PickClass = function (a) {
	return {$: 'PickClass', a: a};
};
var $author$project$Main$ReadFile = function (a) {
	return {$: 'ReadFile', a: a};
};
var $elm$core$Array$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, list);
			var jsArray = _v0.a;
			var remainingItems = _v0.b;
			if (_Utils_cmp(
				$elm$core$Elm$JsArray$length(jsArray),
				$elm$core$Array$branchFactor) < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					true,
					{nodeList: nodeList, nodeListSize: nodeListSize, tail: jsArray});
			} else {
				var $temp$list = remainingItems,
					$temp$nodeList = A2(
					$elm$core$List$cons,
					$elm$core$Array$Leaf(jsArray),
					nodeList),
					$temp$nodeListSize = nodeListSize + 1;
				list = $temp$list;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue fromListHelp;
			}
		}
	});
var $elm$core$Array$fromList = function (list) {
	if (!list.b) {
		return $elm$core$Array$empty;
	} else {
		return A3($elm$core$Array$fromListHelp, list, _List_Nil, 0);
	}
};
var $elm$core$Elm$JsArray$push = _JsArray_push;
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$core$Array$bitMask = 4294967295 >>> (32 - $elm$core$Array$shiftStep);
var $elm$core$Basics$ge = _Utils_ge;
var $elm$core$Elm$JsArray$singleton = _JsArray_singleton;
var $elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var $elm$core$Elm$JsArray$unsafeSet = _JsArray_unsafeSet;
var $elm$core$Array$insertTailInTree = F4(
	function (shift, index, tail, tree) {
		var pos = $elm$core$Array$bitMask & (index >>> shift);
		if (_Utils_cmp(
			pos,
			$elm$core$Elm$JsArray$length(tree)) > -1) {
			if (shift === 5) {
				return A2(
					$elm$core$Elm$JsArray$push,
					$elm$core$Array$Leaf(tail),
					tree);
			} else {
				var newSub = $elm$core$Array$SubTree(
					A4($elm$core$Array$insertTailInTree, shift - $elm$core$Array$shiftStep, index, tail, $elm$core$Elm$JsArray$empty));
				return A2($elm$core$Elm$JsArray$push, newSub, tree);
			}
		} else {
			var value = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (value.$ === 'SubTree') {
				var subTree = value.a;
				var newSub = $elm$core$Array$SubTree(
					A4($elm$core$Array$insertTailInTree, shift - $elm$core$Array$shiftStep, index, tail, subTree));
				return A3($elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
			} else {
				var newSub = $elm$core$Array$SubTree(
					A4(
						$elm$core$Array$insertTailInTree,
						shift - $elm$core$Array$shiftStep,
						index,
						tail,
						$elm$core$Elm$JsArray$singleton(value)));
				return A3($elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
			}
		}
	});
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $elm$core$Array$unsafeReplaceTail = F2(
	function (newTail, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		var originalTailLen = $elm$core$Elm$JsArray$length(tail);
		var newTailLen = $elm$core$Elm$JsArray$length(newTail);
		var newArrayLen = len + (newTailLen - originalTailLen);
		if (_Utils_eq(newTailLen, $elm$core$Array$branchFactor)) {
			var overflow = _Utils_cmp(newArrayLen >>> $elm$core$Array$shiftStep, 1 << startShift) > 0;
			if (overflow) {
				var newShift = startShift + $elm$core$Array$shiftStep;
				var newTree = A4(
					$elm$core$Array$insertTailInTree,
					newShift,
					len,
					newTail,
					$elm$core$Elm$JsArray$singleton(
						$elm$core$Array$SubTree(tree)));
				return A4($elm$core$Array$Array_elm_builtin, newArrayLen, newShift, newTree, $elm$core$Elm$JsArray$empty);
			} else {
				return A4(
					$elm$core$Array$Array_elm_builtin,
					newArrayLen,
					startShift,
					A4($elm$core$Array$insertTailInTree, startShift, len, newTail, tree),
					$elm$core$Elm$JsArray$empty);
			}
		} else {
			return A4($elm$core$Array$Array_elm_builtin, newArrayLen, startShift, tree, newTail);
		}
	});
var $elm$core$Array$push = F2(
	function (a, array) {
		var tail = array.d;
		return A2(
			$elm$core$Array$unsafeReplaceTail,
			A2($elm$core$Elm$JsArray$push, a, tail),
			array);
	});
var $author$project$TypeDict$hash = F2(
	function (_v0, k) {
		var hasher = _v0.a.hasher;
		return hasher(k);
	});
var $author$project$TypeDict$mapInner = F2(
	function (f, _v0) {
		var inner = _v0.a.inner;
		var hasher = _v0.a.hasher;
		return $author$project$TypeDict$Dict(
			{
				hasher: hasher,
				inner: f(inner)
			});
	});
var $author$project$TypeDict$update = F3(
	function (key, f, dict) {
		return A2(
			$author$project$TypeDict$mapInner,
			A2(
				$elm$core$Dict$update,
				A2($author$project$TypeDict$hash, dict, key),
				function (maybeKVPair) {
					if (maybeKVPair.$ === 'Just') {
						var _v1 = maybeKVPair.a;
						var v = _v1.b;
						return A2(
							$elm$core$Maybe$map,
							$elm$core$Tuple$pair(key),
							f(
								$elm$core$Maybe$Just(v)));
					} else {
						return A2(
							$elm$core$Maybe$map,
							$elm$core$Tuple$pair(key),
							f($elm$core$Maybe$Nothing));
					}
				}),
			dict);
	});
var $author$project$Boon$Knack$insert = F2(
	function (key, val) {
		return A2(
			$author$project$TypeDict$update,
			key,
			function (v) {
				if (v.$ === 'Nothing') {
					return $elm$core$Maybe$Just(
						$elm$core$Array$fromList(
							_List_fromArray(
								[val])));
				} else {
					var ks = v.a;
					return $elm$core$Maybe$Just(
						A2($elm$core$Array$push, val, ks));
				}
			});
	});
var $author$project$TypeDict$insert = F3(
	function (key, val, dict) {
		return A2(
			$author$project$TypeDict$mapInner,
			A2(
				$elm$core$Dict$insert,
				A2($author$project$TypeDict$hash, dict, key),
				_Utils_Tuple2(key, val)),
			dict);
	});
var $author$project$TypeDict$toInner = function (_v0) {
	var inner = _v0.a.inner;
	return inner;
};
var $author$project$TypeDict$delegate = F2(
	function (method, dict) {
		return method(
			$author$project$TypeDict$toInner(dict));
	});
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (_v0.$ === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var $author$project$TypeDict$member = F2(
	function (key, dict) {
		return A2(
			$author$project$TypeDict$delegate,
			$elm$core$Dict$member(
				A2($author$project$TypeDict$hash, dict, key)),
			dict);
	});
var $author$project$Character$applyGainDomains = F2(
	function (domains, character) {
		var _v0 = _Utils_Tuple2(character.domains, character.domainKnacks);
		var oldDomains = _v0.a;
		var oldKnacks = _v0.b;
		var _v1 = A3(
			$elm$core$List$foldl,
			F2(
				function (s, _v2) {
					var os = _v2.a;
					var ok = _v2.b;
					return A2($author$project$TypeDict$member, s, os) ? _Utils_Tuple2(
						os,
						A3($author$project$Boon$Knack$insert, s, '', ok)) : _Utils_Tuple2(
						A3($author$project$TypeDict$insert, s, true, os),
						ok);
				}),
			_Utils_Tuple2(oldDomains, oldKnacks),
			domains);
		var newDomains = _v1.a;
		var newKnacks = _v1.b;
		return _Utils_update(
			character,
			{domainKnacks: newKnacks, domains: newDomains});
	});
var $author$project$Character$applyGainSkills = F2(
	function (skills, character) {
		var _v0 = _Utils_Tuple2(character.skills, character.skillKnacks);
		var oldSkills = _v0.a;
		var oldKnacks = _v0.b;
		var _v1 = A3(
			$elm$core$List$foldl,
			F2(
				function (s, _v2) {
					var os = _v2.a;
					var ok = _v2.b;
					return A2($author$project$TypeDict$member, s, os) ? _Utils_Tuple2(
						os,
						A3($author$project$Boon$Knack$insert, s, '', ok)) : _Utils_Tuple2(
						A3($author$project$TypeDict$insert, s, true, os),
						ok);
				}),
			_Utils_Tuple2(oldSkills, oldKnacks),
			skills);
		var newSkills = _v1.a;
		var newKnacks = _v1.b;
		return _Utils_update(
			character,
			{skillKnacks: newKnacks, skills: newSkills});
	});
var $elm$core$Basics$clamp = F3(
	function (low, high, number) {
		return (_Utils_cmp(number, low) < 0) ? low : ((_Utils_cmp(number, high) > 0) ? high : number);
	});
var $author$project$Character$applyBoon = F2(
	function (boon, character) {
		switch (boon.$) {
			case 'GainResistance':
				var resistance = boon.a;
				var bonus = boon.b;
				return _Utils_update(
					character,
					{
						resistances: A3(
							$author$project$TypeDict$update,
							resistance,
							$elm$core$Maybe$map(
								A2(
									$elm$core$Basics$composeR,
									$elm$core$Basics$add(bonus),
									A2($elm$core$Basics$clamp, 0, 5))),
							character.resistances)
					});
			case 'GainDomains':
				var domains = boon.a;
				return A2($author$project$Character$applyGainDomains, domains, character);
			case 'GainSkills':
				var skills = boon.a;
				return A2($author$project$Character$applyGainSkills, skills, character);
			case 'GainEquipment':
				var equipment = boon.a;
				return _Utils_update(
					character,
					{
						equipment: character.equipment + (A2($elm$core$String$join, '\n\n', equipment) + '\n\n')
					});
			case 'GainRefresh':
				var refresh = boon.a;
				return _Utils_update(
					character,
					{
						refresh: character.refresh + (A2($elm$core$String$join, '\n\n', refresh) + '\n\n')
					});
			case 'GainSkillKnack':
				var skill = boon.a;
				return _Utils_update(
					character,
					{
						skillKnacks: A3($author$project$Boon$Knack$insert, skill, '', character.skillKnacks)
					});
			default:
				var domain = boon.a;
				return _Utils_update(
					character,
					{
						domainKnacks: A3($author$project$Boon$Knack$insert, domain, '', character.domainKnacks)
					});
		}
	});
var $author$project$Character$applyBoons = F2(
	function (boons, character) {
		return A3($elm$core$List$foldl, $author$project$Character$applyBoon, character, boons);
	});
var $author$project$Character$applyAssignment = F2(
	function (_v0, character) {
		var name = _v0.name;
		var boons = _v0.boons;
		return A2(
			$author$project$Character$applyBoons,
			boons,
			_Utils_update(
				character,
				{assignment: name}));
	});
var $author$project$Character$addAbilities = F2(
	function (abilities, character) {
		return A3(
			$elm$core$List$foldl,
			function (ability) {
				return $author$project$Character$applyBoons(ability.boons);
			},
			_Utils_update(
				character,
				{
					abilities: A3(
						$elm$core$List$foldl,
						function (ability) {
							return A2($elm$core$Dict$insert, ability.name, ability);
						},
						character.abilities,
						abilities)
				}),
			abilities);
	});
var $author$project$Character$applyClass = F2(
	function (_v0, character) {
		var name = _v0.name;
		var boons = _v0.boons;
		var coreAbilities = _v0.coreAbilities;
		return A2(
			$author$project$Character$addAbilities,
			coreAbilities,
			A2(
				$author$project$Character$applyBoons,
				boons,
				_Utils_update(
					character,
					{_class: name})));
	});
var $author$project$Character$blank = {abilities: $elm$core$Dict$empty, assignment: '', bonds: '', _class: '', domainKnacks: $author$project$Boon$Knack$newDomains, domains: $author$project$Boon$Domain$new, equipment: '', fallout: '', name: '', notes: '', refresh: '', resistances: $author$project$Boon$Resistance$new, skillKnacks: $author$project$Boon$Knack$newSkills, skills: $author$project$Boon$Skill$new};
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$json$Json$Encode$dict = F3(
	function (toKey, toValue, dictionary) {
		return _Json_wrap(
			A3(
				$elm$core$Dict$foldl,
				F3(
					function (key, value, obj) {
						return A3(
							_Json_addField,
							toKey(key),
							toValue(value),
							obj);
					}),
				_Json_emptyObject(_Utils_Tuple0),
				dictionary));
	});
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$Boon$Domain$encodeDomain = function (domain) {
	return $elm$json$Json$Encode$string(
		$author$project$Boon$Domain$toString(domain));
};
var $author$project$Boon$Resistance$encodeResistance = function (resistance) {
	return $elm$json$Json$Encode$string(
		$author$project$Boon$Resistance$toString(resistance));
};
var $author$project$Boon$Skill$encodeSkill = function (skill) {
	return $elm$json$Json$Encode$string(
		$author$project$Boon$Skill$toString(skill));
};
var $elm$json$Json$Encode$int = _Json_wrap;
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var $author$project$Boon$encode = function (boon) {
	return $elm$json$Json$Encode$object(
		function () {
			switch (boon.$) {
				case 'GainResistance':
					var resistance = boon.a;
					var bonus = boon.b;
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'resistance',
							$author$project$Boon$Resistance$encodeResistance(resistance)),
							_Utils_Tuple2(
							'bonus',
							$elm$json$Json$Encode$int(bonus))
						]);
				case 'GainDomains':
					var domain = boon.a;
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'domain',
							A2($elm$json$Json$Encode$list, $author$project$Boon$Domain$encodeDomain, domain))
						]);
				case 'GainSkills':
					var skill = boon.a;
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'skill',
							A2($elm$json$Json$Encode$list, $author$project$Boon$Skill$encodeSkill, skill))
						]);
				case 'GainEquipment':
					var equipment = boon.a;
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'equipment',
							A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, equipment))
						]);
				case 'GainRefresh':
					var refresh = boon.a;
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'refresh',
							A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, refresh))
						]);
				case 'GainSkillKnack':
					var skill = boon.a;
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'knack',
							$elm$json$Json$Encode$string(
								$author$project$Boon$Skill$toString(skill)))
						]);
				default:
					var domain = boon.a;
					return _List_fromArray(
						[
							_Utils_Tuple2(
							'knack',
							$elm$json$Json$Encode$string(
								$author$project$Boon$Domain$toString(domain)))
						]);
			}
		}());
};
var $author$project$Ability$encode = function (ability) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'name',
				$elm$json$Json$Encode$string(ability.name)),
				_Utils_Tuple2(
				'flavor',
				A2(
					$elm$core$Maybe$withDefault,
					$elm$json$Json$Encode$null,
					A2($elm$core$Maybe$map, $elm$json$Json$Encode$string, ability.flavor))),
				_Utils_Tuple2(
				'boons',
				A2($elm$json$Json$Encode$list, $author$project$Boon$encode, ability.boons)),
				_Utils_Tuple2(
				'text',
				$elm$json$Json$Encode$string(ability.text))
			]));
};
var $elm$json$Json$Encode$bool = _Json_wrap;
var $author$project$TypeDict$Json$Encode$encodeKVPair = F3(
	function (keyEncoder, valEncoder, _v0) {
		var k = _v0.a;
		var v = _v0.b;
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'key',
					keyEncoder(k)),
					_Utils_Tuple2(
					'val',
					valEncoder(v))
				]));
	});
var $author$project$TypeDict$Json$Encode$encoder = F4(
	function (toKey, keyEncoder, valEncoder, dict) {
		return A3(
			$elm$json$Json$Encode$dict,
			toKey,
			A2($author$project$TypeDict$Json$Encode$encodeKVPair, keyEncoder, valEncoder),
			$author$project$TypeDict$toInner(dict));
	});
var $author$project$TypeDict$Json$Encode$string = $author$project$TypeDict$Json$Encode$encoder($elm$core$Basics$identity);
var $author$project$Boon$Domain$encode = A2($author$project$TypeDict$Json$Encode$string, $author$project$Boon$Domain$encodeDomain, $elm$json$Json$Encode$bool);
var $elm$core$Elm$JsArray$foldl = _JsArray_foldl;
var $elm$core$Array$foldl = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldl, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldl, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldl,
			func,
			A3($elm$core$Elm$JsArray$foldl, helper, baseCase, tree),
			tail);
	});
var $elm$json$Json$Encode$array = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$Array$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var $author$project$TypeDict$toHasher = function (_v0) {
	var hasher = _v0.a.hasher;
	return hasher;
};
var $author$project$TypeDict$toStringer = $author$project$TypeDict$toHasher;
var $author$project$Boon$Knack$encode = function (knacks) {
	return A3(
		$author$project$TypeDict$Json$Encode$string,
		A2(
			$elm$core$Basics$composeR,
			$author$project$TypeDict$toStringer(knacks),
			$elm$json$Json$Encode$string),
		$elm$json$Json$Encode$array($elm$json$Json$Encode$string),
		knacks);
};
var $author$project$Boon$Resistance$encode = A2($author$project$TypeDict$Json$Encode$string, $author$project$Boon$Resistance$encodeResistance, $elm$json$Json$Encode$int);
var $author$project$Boon$Skill$encode = A2($author$project$TypeDict$Json$Encode$string, $author$project$Boon$Skill$encodeSkill, $elm$json$Json$Encode$bool);
var $author$project$Character$encode = function (character) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'name',
				$elm$json$Json$Encode$string(character.name)),
				_Utils_Tuple2(
				'class',
				$elm$json$Json$Encode$string(character._class)),
				_Utils_Tuple2(
				'assignment',
				$elm$json$Json$Encode$string(character.assignment)),
				_Utils_Tuple2(
				'skillKnacks',
				$author$project$Boon$Knack$encode(character.skillKnacks)),
				_Utils_Tuple2(
				'domainKnacks',
				$author$project$Boon$Knack$encode(character.domainKnacks)),
				_Utils_Tuple2(
				'equipment',
				$elm$json$Json$Encode$string(character.equipment)),
				_Utils_Tuple2(
				'abilities',
				A3($elm$json$Json$Encode$dict, $elm$core$Basics$identity, $author$project$Ability$encode, character.abilities)),
				_Utils_Tuple2(
				'fallout',
				$elm$json$Json$Encode$string(character.fallout)),
				_Utils_Tuple2(
				'refresh',
				$elm$json$Json$Encode$string(character.refresh)),
				_Utils_Tuple2(
				'bonds',
				$elm$json$Json$Encode$string(character.bonds)),
				_Utils_Tuple2(
				'skills',
				$author$project$Boon$Skill$encode(character.skills)),
				_Utils_Tuple2(
				'domains',
				$author$project$Boon$Domain$encode(character.domains)),
				_Utils_Tuple2(
				'resistances',
				$author$project$Boon$Resistance$encode(character.resistances)),
				_Utils_Tuple2(
				'notes',
				$elm$json$Json$Encode$string(character.notes))
			]));
};
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $elm$file$File$Select$file = F2(
	function (mimes, toMsg) {
		return A2(
			$elm$core$Task$perform,
			toMsg,
			_File_uploadOne(mimes));
	});
var $author$project$Session$fromDisk = F2(
	function (newCharacter, session) {
		if (session.$ === 'NoCharacter') {
			var key = session.a;
			return $author$project$Session$Character(
				{changes: $author$project$Session$Saved, character: newCharacter, navKey: key});
		} else {
			var val = session.a;
			return $author$project$Session$Character(
				_Utils_update(
					val,
					{changes: $author$project$Session$Saved, character: newCharacter}));
		}
	});
var $elm$browser$Browser$Navigation$load = _Browser_load;
var $elm$core$Debug$log = _Debug_log;
var $elm$core$Tuple$mapBoth = F3(
	function (funcA, funcB, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			funcA(x),
			funcB(y));
	});
var $elm$core$Tuple$mapFirst = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			func(x),
			y);
	});
var $author$project$Session$navKey = function (session) {
	if (session.$ === 'Character') {
		var val = session.a;
		return val.navKey;
	} else {
		var val = session.a;
		return val;
	}
};
var $elm$browser$Browser$Navigation$pushUrl = _Browser_pushUrl;
var $elm$browser$Browser$Navigation$replaceUrl = _Browser_replaceUrl;
var $author$project$Session$encodeChanges = function (cs) {
	return $elm$json$Json$Encode$int(
		function () {
			switch (cs.$) {
				case 'Unsaved':
					return 0;
				case 'SavedLocally':
					return 1;
				default:
					return 2;
			}
		}());
};
var $author$project$Session$encoder = function (session) {
	if (session.$ === 'NoCharacter') {
		return $elm$json$Json$Encode$null;
	} else {
		var val = session.a;
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'character',
					$author$project$Character$encode(val.character)),
					_Utils_Tuple2(
					'changes',
					$author$project$Session$encodeChanges(val.changes))
				]));
	}
};
var $author$project$Session$savedChangesLocally = function (session) {
	if (session.$ === 'NoCharacter') {
		return session;
	} else {
		var val = session.a;
		return $author$project$Session$Character(
			_Utils_update(
				val,
				{
					changes: function () {
						var _v1 = val.changes;
						switch (_v1.$) {
							case 'Unsaved':
								return $author$project$Session$SavedLocally;
							case 'SavedLocally':
								return $author$project$Session$SavedLocally;
							default:
								return $author$project$Session$Saved;
						}
					}()
				}));
	}
};
var $author$project$Ports$storeCharacter = _Platform_outgoingPort('storeCharacter', $elm$json$Json$Encode$string);
var $author$project$Session$save = function (session) {
	if (session.$ === 'NoCharacter') {
		return _Utils_Tuple2(session, $elm$core$Platform$Cmd$none);
	} else {
		var updatedSession = $author$project$Session$savedChangesLocally(session);
		return _Utils_Tuple2(
			$author$project$Session$savedChangesLocally(updatedSession),
			$author$project$Ports$storeCharacter(
				A2(
					$elm$json$Json$Encode$encode,
					2,
					$author$project$Session$encoder(updatedSession))));
	}
};
var $author$project$Session$savedChanges = function (session) {
	if (session.$ === 'NoCharacter') {
		return session;
	} else {
		var val = session.a;
		return $author$project$Session$Character(
			_Utils_update(
				val,
				{changes: $author$project$Session$Saved}));
	}
};
var $author$project$Ports$savedCharacter = _Platform_outgoingPort(
	'savedCharacter',
	function ($) {
		return $elm$json$Json$Encode$null;
	});
var $author$project$Session$setCharacter = F2(
	function (newCharacter, session) {
		if (session.$ === 'NoCharacter') {
			var key = session.a;
			return $author$project$Session$Character(
				{changes: $author$project$Session$Unsaved, character: newCharacter, navKey: key});
		} else {
			var val = session.a;
			return $author$project$Session$Character(
				_Utils_update(
					val,
					{changes: $author$project$Session$Unsaved, character: newCharacter}));
		}
	});
var $elm$file$File$Download$string = F3(
	function (name, mime, content) {
		return A2(
			$elm$core$Task$perform,
			$elm$core$Basics$never,
			A3(_File_download, name, mime, content));
	});
var $elm$file$File$toString = _File_toString;
var $elm$url$Url$addPort = F2(
	function (maybePort, starter) {
		if (maybePort.$ === 'Nothing') {
			return starter;
		} else {
			var port_ = maybePort.a;
			return starter + (':' + $elm$core$String$fromInt(port_));
		}
	});
var $elm$url$Url$addPrefixed = F3(
	function (prefix, maybeSegment, starter) {
		if (maybeSegment.$ === 'Nothing') {
			return starter;
		} else {
			var segment = maybeSegment.a;
			return _Utils_ap(
				starter,
				_Utils_ap(prefix, segment));
		}
	});
var $elm$url$Url$toString = function (url) {
	var http = function () {
		var _v0 = url.protocol;
		if (_v0.$ === 'Http') {
			return 'http://';
		} else {
			return 'https://';
		}
	}();
	return A3(
		$elm$url$Url$addPrefixed,
		'#',
		url.fragment,
		A3(
			$elm$url$Url$addPrefixed,
			'?',
			url.query,
			_Utils_ap(
				A2(
					$elm$url$Url$addPort,
					url.port_,
					_Utils_ap(http, url.host)),
				url.path)));
};
var $author$project$Status$Failed = function (a) {
	return {$: 'Failed', a: a};
};
var $author$project$Status$Loaded = function (a) {
	return {$: 'Loaded', a: a};
};
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$core$Dict$diff = F2(
	function (t1, t2) {
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (k, v, t) {
					return A2($elm$core$Dict$remove, k, t);
				}),
			t1,
			t2);
	});
var $author$project$Abilities$dedup = F2(
	function (abilities, advances) {
		return _Utils_update(
			advances,
			{
				high: A2($elm$core$Dict$diff, advances.high, abilities),
				low: A2($elm$core$Dict$diff, advances.low, abilities),
				medium: A2($elm$core$Dict$diff, advances.medium, abilities)
			});
	});
var $author$project$Abilities$GotAdvances = F2(
	function (a, b) {
		return {$: 'GotAdvances', a: a, b: b};
	});
var $author$project$Abilities$Advances = F3(
	function (low, medium, high) {
		return {high: high, low: low, medium: medium};
	});
var $elm$json$Json$Decode$map3 = _Json_map3;
var $author$project$Abilities$decoder = function () {
	var listToDict = A2(
		$elm$core$List$foldl,
		function (ability) {
			return A2($elm$core$Dict$insert, ability.name, ability);
		},
		$elm$core$Dict$empty);
	return A4(
		$elm$json$Json$Decode$map3,
		$author$project$Abilities$Advances,
		A2(
			$elm$json$Json$Decode$map,
			listToDict,
			A2(
				$elm$json$Json$Decode$field,
				'low',
				$elm$json$Json$Decode$list($author$project$Ability$decoder))),
		A2(
			$elm$json$Json$Decode$map,
			listToDict,
			A2(
				$elm$json$Json$Decode$field,
				'medium',
				$elm$json$Json$Decode$list($author$project$Ability$decoder))),
		A2(
			$elm$json$Json$Decode$map,
			listToDict,
			A2(
				$elm$json$Json$Decode$field,
				'high',
				$elm$json$Json$Decode$list($author$project$Ability$decoder))));
}();
var $author$project$Abilities$fetchFromList = function (metadata) {
	return $elm$core$Platform$Cmd$batch(
		A2(
			$elm$core$List$map,
			function (_v0) {
				var name = _v0.a;
				var loc = _v0.b;
				return $elm$http$Http$get(
					{
						expect: A2(
							$elm$http$Http$expectJson,
							$author$project$Abilities$GotAdvances(name),
							$author$project$Abilities$decoder),
						url: _Utils_ap($author$project$Abilities$location, loc)
					});
			},
			$elm$core$Dict$toList(metadata)));
};
var $author$project$Status$fromResult = function (result) {
	if (result.$ === 'Ok') {
		var val = result.a;
		return $author$project$Status$Loaded(val);
	} else {
		var e = result.a;
		return $author$project$Status$Failed(e);
	}
};
var $elm$core$Dict$map = F2(
	function (func, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				A2(func, key, value),
				A2($elm$core$Dict$map, func, left),
				A2($elm$core$Dict$map, func, right));
		}
	});
var $author$project$Status$map = F2(
	function (f, status) {
		switch (status.$) {
			case 'Loaded':
				var a = status.a;
				return $author$project$Status$Loaded(
					f(a));
			case 'Loading':
				return $author$project$Status$Loading;
			default:
				var x = status.a;
				return $author$project$Status$Failed(x);
		}
	});
var $elm$core$Dict$values = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return A2($elm$core$List$cons, value, valueList);
			}),
		_List_Nil,
		dict);
};
var $author$project$Abilities$update = F2(
	function (msg, abilities) {
		var maybeCharacter = $author$project$Session$character(abilities.session);
		switch (msg.$) {
			case 'ChoseAbility':
				var ability = msg.a;
				var _v1 = A2($elm$core$Debug$log, 'Chose ability', _Utils_Tuple0);
				return _Utils_Tuple2(
					_Utils_update(
						abilities,
						{
							chosen: A3($elm$core$Dict$insert, ability.name, ability, abilities.chosen)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UnchoseAbility':
				var name = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						abilities,
						{
							chosen: A2($elm$core$Dict$remove, name, abilities.chosen)
						}),
					$elm$core$Platform$Cmd$none);
			case 'GotMetadata':
				if (msg.a.$ === 'Ok') {
					var metadata = msg.a.a;
					return _Utils_Tuple2(
						_Utils_update(
							abilities,
							{
								metadata: $author$project$Status$Loaded(metadata),
								tabs: A2(
									$elm$core$Dict$map,
									$elm$core$Basics$always(
										$elm$core$Basics$always($author$project$Status$Loading)),
									metadata)
							}),
						$author$project$Abilities$fetchFromList(metadata));
				} else {
					var err = msg.a.a;
					return _Utils_Tuple2(
						_Utils_update(
							abilities,
							{
								metadata: $author$project$Status$Failed(err)
							}),
						$elm$core$Platform$Cmd$none);
				}
			case 'GotAdvances':
				var name = msg.a;
				var advances = msg.b;
				return _Utils_Tuple2(
					_Utils_update(
						abilities,
						{
							tabs: A3(
								$elm$core$Dict$insert,
								name,
								A2(
									$author$project$Status$map,
									A2(
										$elm$core$Maybe$withDefault,
										$elm$core$Basics$identity,
										A2(
											$elm$core$Maybe$map,
											$author$project$Abilities$dedup,
											A2(
												$elm$core$Maybe$map,
												function ($) {
													return $.abilities;
												},
												maybeCharacter))),
									$author$project$Status$fromResult(advances)),
								abilities.tabs)
						}),
					$elm$core$Platform$Cmd$none);
			default:
				var _v2 = A2(
					$elm$core$Maybe$map,
					$author$project$Character$addAbilities(
						$elm$core$Dict$values(abilities.chosen)),
					maybeCharacter);
				if (_v2.$ === 'Nothing') {
					return _Utils_Tuple2(abilities, $elm$core$Platform$Cmd$none);
				} else {
					var updatedCharacter = _v2.a;
					var session = A2($author$project$Session$setCharacter, updatedCharacter, abilities.session);
					return A2(
						$elm$core$Tuple$mapFirst,
						function (s) {
							return _Utils_update(
								abilities,
								{
									session: s,
									tabs: A2(
										$elm$core$Dict$map,
										function (_v3) {
											return $author$project$Status$map(
												$author$project$Abilities$dedup(updatedCharacter.abilities));
										},
										abilities.tabs)
								});
						},
						$author$project$Session$save(session));
				}
		}
	});
var $author$project$Character$update = F2(
	function (msg, character) {
		switch (msg.$) {
			case 'UpdatedName':
				var name = msg.a;
				return _Utils_update(
					character,
					{name: name});
			case 'UpdatedSkills':
				var skills = msg.a;
				return _Utils_update(
					character,
					{skills: skills});
			case 'UpdatedDomains':
				var domains = msg.a;
				return _Utils_update(
					character,
					{domains: domains});
			case 'UpdatedSkillKnacks':
				var knacks = msg.a;
				return _Utils_update(
					character,
					{skillKnacks: knacks});
			case 'UpdatedDomainKnacks':
				var knacks = msg.a;
				return _Utils_update(
					character,
					{domainKnacks: knacks});
			case 'UpdatedRefresh':
				var refresh = msg.a;
				return _Utils_update(
					character,
					{refresh: refresh});
			case 'UpdatedEquipment':
				var equipment = msg.a;
				return _Utils_update(
					character,
					{equipment: equipment});
			case 'UpdatedFallout':
				var fallout = msg.a;
				return _Utils_update(
					character,
					{fallout: fallout});
			case 'UpdatedBonds':
				var bonds = msg.a;
				return _Utils_update(
					character,
					{bonds: bonds});
			case 'UpdatedResistances':
				var resistances = msg.a;
				return _Utils_update(
					character,
					{resistances: resistances});
			case 'UpdatedNotes':
				var notes = msg.a;
				return _Utils_update(
					character,
					{notes: notes});
			default:
				return character;
		}
	});
var $author$project$PlayAids$update = F2(
	function (result, playAids) {
		return _Utils_update(
			playAids,
			{
				aids: $author$project$Status$fromResult(result)
			});
	});
var $author$project$Abilities$updateSession = F2(
	function (session, abilities) {
		return _Utils_update(
			abilities,
			{session: session});
	});
var $author$project$Main$updateSession = F2(
	function (session, model) {
		switch (model.$) {
			case 'PickClass':
				return $author$project$Main$PickClass(session);
			case 'PickAssignment':
				var val = model.b;
				return A2($author$project$Main$PickAssignment, session, val);
			case 'Character':
				return $author$project$Main$Character(session);
			case 'DecodeErr':
				var err = model.b;
				return A2($author$project$Main$DecodeErr, session, err);
			case 'Abilities':
				var abilities = model.a;
				return $author$project$Main$Abilities(
					A2($author$project$Abilities$updateSession, session, abilities));
			case 'Landing':
				return $author$project$Main$Landing(session);
			default:
				var aid = model.b;
				return A2($author$project$Main$PlayAid, session, aid);
		}
	});
var $author$project$Main$update = F2(
	function (msg, model) {
		var _v0 = _Utils_Tuple2(model, msg);
		_v0$13:
		while (true) {
			switch (_v0.b.$) {
				case 'ClickedNewCharacter':
					if (_v0.a.$ === 'Landing') {
						var session = _v0.a.a;
						var _v1 = _v0.b;
						return _Utils_Tuple2(
							$author$project$Main$PickClass(session),
							$elm$core$Platform$Cmd$none);
					} else {
						break _v0$13;
					}
				case 'PickedClass':
					if (_v0.a.$ === 'PickClass') {
						var session = _v0.a.a;
						var _class = _v0.b.a;
						return _Utils_Tuple2(
							A2($author$project$Main$PickAssignment, session, _class),
							$elm$core$Platform$Cmd$none);
					} else {
						break _v0$13;
					}
				case 'PickedAssignment':
					if (_v0.a.$ === 'PickAssignment') {
						var _v2 = _v0.a;
						var session = _v2.a;
						var _class = _v2.b;
						var assignment = _v0.b.a;
						var character = A2(
							$author$project$Character$applyAssignment,
							assignment,
							A2($author$project$Character$applyClass, _class, $author$project$Character$blank));
						var updatedSession = A2($author$project$Session$setCharacter, character, session);
						return A2(
							$elm$core$Tuple$mapFirst,
							$author$project$Main$Character,
							$author$project$Session$save(updatedSession));
					} else {
						break _v0$13;
					}
				case 'ClickedSave':
					if (_v0.a.$ === 'Character') {
						var session = _v0.a.a;
						var _v3 = _v0.b;
						var _v4 = $author$project$Session$save(
							$author$project$Session$savedChanges(session));
						var updatedSession = _v4.a;
						var cmd = _v4.b;
						return _Utils_Tuple2(
							$author$project$Main$Character(updatedSession),
							function () {
								var _v5 = $author$project$Session$character(session);
								if (_v5.$ === 'Just') {
									var character = _v5.a;
									return $elm$core$Platform$Cmd$batch(
										_List_fromArray(
											[
												A3(
												$elm$file$File$Download$string,
												character.name,
												'application/json',
												A2(
													$elm$json$Json$Encode$encode,
													2,
													$author$project$Character$encode(character))),
												$author$project$Ports$savedCharacter(_Utils_Tuple0),
												cmd
											]));
								} else {
									return $elm$core$Platform$Cmd$none;
								}
							}());
					} else {
						break _v0$13;
					}
				case 'CharacterMsg':
					if (_v0.a.$ === 'Character') {
						var session = _v0.a.a;
						var subMsg = _v0.b.a;
						var _v6 = $author$project$Session$character(session);
						if (_v6.$ === 'Just') {
							var character = _v6.a;
							var _v7 = $author$project$Session$save(
								A2(
									$author$project$Session$setCharacter,
									A2($author$project$Character$update, subMsg, character),
									session));
							var updatedSession = _v7.a;
							var cmd = _v7.b;
							return _Utils_Tuple2(
								$author$project$Main$Character(updatedSession),
								$elm$core$Platform$Cmd$batch(
									_List_fromArray(
										[
											$author$project$Ports$updatedCharacter(_Utils_Tuple0),
											cmd
										])));
						} else {
							return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
						}
					} else {
						break _v0$13;
					}
				case 'AbilitiesMsg':
					if (_v0.a.$ === 'Abilities') {
						var abilities = _v0.a.a;
						var subMsg = _v0.b.a;
						return A3(
							$elm$core$Tuple$mapBoth,
							$author$project$Main$Abilities,
							$elm$core$Platform$Cmd$map($author$project$Main$AbilitiesMsg),
							A2($author$project$Abilities$update, subMsg, abilities));
					} else {
						break _v0$13;
					}
				case 'GotPlayAid':
					if (_v0.a.$ === 'PlayAid') {
						var _v8 = _v0.a;
						var session = _v8.a;
						var playAid = _v8.b;
						var result = _v0.b.a;
						return _Utils_Tuple2(
							A2(
								$author$project$Main$PlayAid,
								session,
								A2($author$project$PlayAids$update, result, playAid)),
							$elm$core$Platform$Cmd$none);
					} else {
						break _v0$13;
					}
				case 'ClickedOpenFile':
					var _v9 = _v0.b;
					return _Utils_Tuple2(
						model,
						A2(
							$elm$file$File$Select$file,
							_List_fromArray(
								['application/json']),
							$author$project$Main$FileLoaded));
				case 'FileLoaded':
					var file = _v0.b.a;
					return _Utils_Tuple2(
						model,
						A2(
							$elm$core$Task$perform,
							$author$project$Main$ReadFile,
							$elm$file$File$toString(file)));
				case 'ReadFile':
					var content = _v0.b.a;
					var _v10 = A2($elm$json$Json$Decode$decodeString, $author$project$Character$decoder, content);
					if (_v10.$ === 'Err') {
						var err = _v10.a;
						return _Utils_Tuple2(
							A2(
								$author$project$Main$DecodeErr,
								$author$project$Main$toSession(model),
								err),
							$elm$core$Platform$Cmd$none);
					} else {
						var character = _v10.a;
						var _v11 = $author$project$Session$save(
							A2(
								$author$project$Session$fromDisk,
								character,
								$author$project$Main$toSession(model)));
						var session = _v11.a;
						var cmd = _v11.b;
						return _Utils_Tuple2(
							$author$project$Main$Character(session),
							cmd);
					}
				case 'LinkClicked':
					var urlRequest = _v0.b.a;
					if (urlRequest.$ === 'Internal') {
						var url = urlRequest.a;
						var urlString = $elm$url$Url$toString(url);
						var route = $author$project$Route$parse(url);
						var navKey = $author$project$Session$navKey(
							$author$project$Main$toSession(model));
						var _v13 = _Utils_Tuple2(model, route);
						_v13$2:
						while (true) {
							switch (_v13.a.$) {
								case 'Abilities':
									if (_v13.b.$ === 'Abilities') {
										return _Utils_Tuple2(
											model,
											A2($elm$browser$Browser$Navigation$replaceUrl, navKey, urlString));
									} else {
										break _v13$2;
									}
								case 'PlayAid':
									if (_v13.b.$ === 'PlayAid') {
										var _v14 = _v13.a;
										var _v15 = _v13.b;
										return _Utils_Tuple2(
											model,
											A2($elm$browser$Browser$Navigation$replaceUrl, navKey, urlString));
									} else {
										break _v13$2;
									}
								default:
									break _v13$2;
							}
						}
						return _Utils_Tuple2(
							model,
							A2($elm$browser$Browser$Navigation$pushUrl, navKey, urlString));
					} else {
						var href = urlRequest.a;
						return _Utils_Tuple2(
							model,
							$elm$browser$Browser$Navigation$load(href));
					}
				case 'UrlChanged':
					var url = _v0.b.a;
					return A2(
						$author$project$Main$changeRoute,
						A2(
							$elm$core$Debug$log,
							'UrlChanged',
							$author$project$Route$parse(url)),
						model);
				case 'SavedChanges':
					var _v16 = _v0.b;
					return _Utils_Tuple2(
						A2(
							$author$project$Main$updateSession,
							$author$project$Session$savedChangesLocally(
								$author$project$Main$toSession(model)),
							model),
						$elm$core$Platform$Cmd$none);
				default:
					break _v0$13;
			}
		}
		return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
	});
var $author$project$Main$CharacterMsg = function (a) {
	return {$: 'CharacterMsg', a: a};
};
var $author$project$Main$ClickedNewCharacter = {$: 'ClickedNewCharacter'};
var $author$project$Main$ClickedOpenFile = {$: 'ClickedOpenFile'};
var $author$project$Main$ClickedSave = {$: 'ClickedSave'};
var $author$project$Main$PickedAssignment = function (a) {
	return {$: 'PickedAssignment', a: a};
};
var $author$project$Main$PickedClass = function (a) {
	return {$: 'PickedClass', a: a};
};
var $elm$html$Html$a = _VirtualDom_node('a');
var $author$project$Boon$dogsbody = {
	boons: _List_fromArray(
		[
			A2($author$project$Boon$GainResistance, $author$project$Boon$Resistance$Resolve, 2),
			$author$project$Boon$GainDomains(
			_List_fromArray(
				[$author$project$Boon$Domain$LowSociety]))
		]),
	name: 'Dogsbody'
};
var $author$project$Boon$escapee = {
	boons: _List_fromArray(
		[
			A2($author$project$Boon$GainResistance, $author$project$Boon$Resistance$Shadow, 2),
			$author$project$Boon$GainSkills(
			_List_fromArray(
				[$author$project$Boon$Skill$Skulk]))
		]),
	name: 'Escapee'
};
var $author$project$Boon$labour = {
	boons: _List_fromArray(
		[
			A2($author$project$Boon$GainResistance, $author$project$Boon$Resistance$Body, 2),
			$author$project$Boon$GainSkills(
			_List_fromArray(
				[$author$project$Boon$Skill$Steal]))
		]),
	name: 'Labour'
};
var $author$project$Boon$personalAssistant = {
	boons: _List_fromArray(
		[
			A2($author$project$Boon$GainResistance, $author$project$Boon$Resistance$Resources, 2),
			$author$project$Boon$GainSkills(
			_List_fromArray(
				[$author$project$Boon$Skill$Investigate]))
		]),
	name: 'Personal Assistant'
};
var $author$project$Boon$pleasure = {
	boons: _List_fromArray(
		[
			A2($author$project$Boon$GainResistance, $author$project$Boon$Resistance$Resources, 2),
			$author$project$Boon$GainDomains(
			_List_fromArray(
				[$author$project$Boon$Domain$HighSociety]))
		]),
	name: 'Pleasure'
};
var $author$project$Boon$searchAndRescue = {
	boons: _List_fromArray(
		[
			A2($author$project$Boon$GainResistance, $author$project$Boon$Resistance$Resolve, 2),
			$author$project$Boon$GainSkills(
			_List_fromArray(
				[$author$project$Boon$Skill$Resist]))
		]),
	name: 'Search and Rescue'
};
var $author$project$Boon$security = {
	boons: _List_fromArray(
		[
			A2($author$project$Boon$GainResistance, $author$project$Boon$Resistance$Body, 2),
			$author$project$Boon$GainSkills(
			_List_fromArray(
				[$author$project$Boon$Skill$Scrap]))
		]),
	name: 'Security'
};
var $author$project$Boon$spy = {
	boons: _List_fromArray(
		[
			A2($author$project$Boon$GainResistance, $author$project$Boon$Resistance$Shadow, 2),
			$author$project$Boon$GainSkills(
			_List_fromArray(
				[$author$project$Boon$Skill$Deceive]))
		]),
	name: 'Spy'
};
var $author$project$Boon$testSubject = {
	boons: _List_fromArray(
		[
			$author$project$Boon$GainSkills(
			_List_fromArray(
				[$author$project$Boon$Skill$Resist])),
			$author$project$Boon$GainDomains(
			_List_fromArray(
				[$author$project$Boon$Domain$Science]))
		]),
	name: 'Test Subject'
};
var $author$project$Boon$assignments = _List_fromArray(
	[$author$project$Boon$labour, $author$project$Boon$personalAssistant, $author$project$Boon$dogsbody, $author$project$Boon$security, $author$project$Boon$spy, $author$project$Boon$testSubject, $author$project$Boon$pleasure, $author$project$Boon$searchAndRescue, $author$project$Boon$escapee]);
var $elm$html$Html$button = _VirtualDom_node('button');
var $author$project$Session$changesToString = function (val) {
	switch (val.$) {
		case 'Unsaved':
			return 'There are unsaved changes.';
		case 'SavedLocally':
			return 'All changes saved locally.';
		default:
			return 'All changes saved to disk.';
	}
};
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$html$Html$Attributes$classList = function (classes) {
	return $elm$html$Html$Attributes$class(
		A2(
			$elm$core$String$join,
			' ',
			A2(
				$elm$core$List$map,
				$elm$core$Tuple$first,
				A2($elm$core$List$filter, $elm$core$Tuple$second, classes))));
};
var $author$project$Class$awoken = {
	boons: _List_fromArray(
		[
			A2($author$project$Boon$GainResistance, $author$project$Boon$Resistance$Resolve, 2),
			A2($author$project$Boon$GainResistance, $author$project$Boon$Resistance$Shadow, 1),
			A2($author$project$Boon$GainResistance, $author$project$Boon$Resistance$Body, 1),
			$author$project$Boon$GainRefresh(
			_List_fromArray(
				['Take something back from those who would oppress you.'])),
			$author$project$Boon$GainEquipment(
			_List_fromArray(
				['A small Ur-artefact whose powers have not revealed themselves to you', 'EITHER A knife (D3, Concealable) OR A staff (D3, Conduit)'])),
			$author$project$Boon$GainSkills(
			_List_fromArray(
				[$author$project$Boon$Skill$Compel, $author$project$Boon$Skill$Resist])),
			$author$project$Boon$GainDomains(
			_List_fromArray(
				[$author$project$Boon$Domain$Science, $author$project$Boon$Domain$Weirdness]))
		]),
	coreAbilities: _List_fromArray(
		[
			{
			boons: _List_Nil,
			flavor: $elm$core$Maybe$Just('You know what people want.'),
			name: 'Heart\'s Desire',
			text: 'Once per situation, pick an NPC that you can observe for a while. The GM will tell you what they want most of all right now.'
		},
			{
			boons: _List_Nil,
			flavor: $elm$core$Maybe$Just('The Way\'s light shines forth from you.'),
			name: 'Moonlight',
			text: 'Your rune glows as brightly as the full moon, casting a calm light into the darkness that cannot be extinguished unless you decide to snuff it, or you fall unconscious.'
		}
		]),
	name: 'Awoken'
};
var $author$project$Class$cloak = {
	boons: _List_fromArray(
		[
			A2($author$project$Boon$GainResistance, $author$project$Boon$Resistance$Shadow, 3),
			A2($author$project$Boon$GainResistance, $author$project$Boon$Resistance$Body, 1),
			$author$project$Boon$GainRefresh(
			_List_fromArray(
				['Show someone they should not have underestimated you'])),
			$author$project$Boon$GainSkills(
			_List_fromArray(
				[$author$project$Boon$Skill$Skulk, $author$project$Boon$Skill$Scrap])),
			$author$project$Boon$GainDomains(
			_List_fromArray(
				[$author$project$Boon$Domain$Criminal, $author$project$Boon$Domain$HighSociety])),
			$author$project$Boon$GainEquipment(
			_List_fromArray(
				['Light body armour (Armour 2)', 'Climbing gear and ropes', 'EITHER Shoddy rifle (D8, Ranged, Reload, Unreliable) and Knife (D3, Concealable) OR Sword (D6) and Meteor Hammer (D3, Ranged, Stunning)']))
		]),
	coreAbilities: _List_fromArray(
		[
			{
			boons: _List_Nil,
			flavor: $elm$core$Maybe$Just('Nothing can keep you out.'),
			name: 'Surprise Infiltration',
			text: 'Once per session, insert yourself into a situation where you are not currently present, so long as there\'s some conceivable way you could get in there.'
		},
			{
			boons: _List_Nil,
			flavor: $elm$core$Maybe$Just('You are a trained infiltrator, and others would do well to heed your words.'),
			name: 'Tactician',
			text: 'When you enter a dangerous situation, you can name up to three features or opportunities that your allies can take advantage of. The first time you or an ally uses an opportunity, they roll with mastery (for example: cover with a good view of the battlefield, an exit, a badly-guarded door, a stack of barrels, etc.).'
		}
		]),
	name: 'Cloak'
};
var $author$project$Class$doc = {
	boons: _List_fromArray(
		[
			A2($author$project$Boon$GainResistance, $author$project$Boon$Resistance$Resolve, 2),
			A2($author$project$Boon$GainResistance, $author$project$Boon$Resistance$Reputation, 2),
			$author$project$Boon$GainRefresh(
			_List_fromArray(
				['Help those who cannot help themselves.'])),
			$author$project$Boon$GainSkills(
			_List_fromArray(
				[$author$project$Boon$Skill$Patch, $author$project$Boon$Skill$Compel])),
			$author$project$Boon$GainDomains(
			_List_fromArray(
				[$author$project$Boon$Domain$Science, $author$project$Boon$Domain$LowSociety])),
			$author$project$Boon$GainEquipment(
			_List_fromArray(
				['The common red medic outfit bearing the official white medic seal of the Hegemony', 'A less than ideally stocked medkit', 'EITHER Scalpel (D3, Concealable) OR Stun gun (D6, One-shot, Stunning)']))
		]),
	coreAbilities: _List_fromArray(
		[
			{
			boons: _List_Nil,
			flavor: $elm$core$Maybe$Just('You take some time to check on your allies and stitch them up if need be.'),
			name: 'Medical Attention',
			text: 'Once per session, help your allies heal, physically and mentally. All allies present may restore 3 stress from Body or Resolve.'
		},
			{
			boons: _List_Nil,
			flavor: $elm$core$Maybe$Just('People are grateful for your help.'),
			name: 'Bedside Manner',
			text: 'When you heal someone, gain a bond with them until the end of the next day.'
		}
		]),
	name: 'Doc'
};
var $author$project$Class$classes = _List_fromArray(
	[$author$project$Class$doc, $author$project$Class$cloak, $author$project$Class$awoken]);
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$html$Html$footer = _VirtualDom_node('footer');
var $elm$html$Html$h1 = _VirtualDom_node('h1');
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var $elm$html$Html$map = $elm$virtual_dom$VirtualDom$map;
var $elm$core$Basics$not = _Basics_not;
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$p = _VirtualDom_node('p');
var $elm$html$Html$span = _VirtualDom_node('span');
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$Abilities$ApplyChosen = {$: 'ApplyChosen'};
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $elm$html$Html$li = _VirtualDom_node('li');
var $elm$html$Html$nav = _VirtualDom_node('nav');
var $elm$url$Url$Builder$Absolute = {$: 'Absolute'};
var $elm$url$Url$Builder$rootToPrePath = function (root) {
	switch (root.$) {
		case 'Absolute':
			return '/';
		case 'Relative':
			return '';
		default:
			var prePath = root.a;
			return prePath + '/';
	}
};
var $elm$url$Url$Builder$toQueryPair = function (_v0) {
	var key = _v0.a;
	var value = _v0.b;
	return key + ('=' + value);
};
var $elm$url$Url$Builder$toQuery = function (parameters) {
	if (!parameters.b) {
		return '';
	} else {
		return '?' + A2(
			$elm$core$String$join,
			'&',
			A2($elm$core$List$map, $elm$url$Url$Builder$toQueryPair, parameters));
	}
};
var $elm$url$Url$Builder$custom = F4(
	function (root, pathSegments, parameters, maybeFragment) {
		var fragmentless = _Utils_ap(
			$elm$url$Url$Builder$rootToPrePath(root),
			_Utils_ap(
				A2($elm$core$String$join, '/', pathSegments),
				$elm$url$Url$Builder$toQuery(parameters)));
		if (maybeFragment.$ === 'Nothing') {
			return fragmentless;
		} else {
			var fragment = maybeFragment.a;
			return fragmentless + ('#' + fragment);
		}
	});
var $author$project$Route$toString = function (route) {
	var absolute = F3(
		function (path, query, frag) {
			return A4(
				$elm$url$Url$Builder$custom,
				$elm$url$Url$Builder$Absolute,
				A2($elm$core$List$cons, 'reves', path),
				query,
				frag);
		});
	switch (route.$) {
		case 'Root':
			return A3(absolute, _List_Nil, _List_Nil, $elm$core$Maybe$Nothing);
		case 'Abilities':
			var selected = route.a;
			return A3(
				absolute,
				_List_fromArray(
					['abilities']),
				_List_Nil,
				selected);
		default:
			var topic = route.a;
			var selected = route.b;
			return A3(
				absolute,
				_List_fromArray(
					[
						'play-aid',
						$author$project$PlayAids$topicToString(topic)
					]),
				_List_Nil,
				selected);
	}
};
var $elm$html$Html$ul = _VirtualDom_node('ul');
var $elm$html$Html$h2 = _VirtualDom_node('h2');
var $author$project$Abilities$ChoseAbility = function (a) {
	return {$: 'ChoseAbility', a: a};
};
var $author$project$Abilities$UnchoseAbility = function (a) {
	return {$: 'UnchoseAbility', a: a};
};
var $elm$html$Html$h3 = _VirtualDom_node('h3');
var $elm$html$Html$i = _VirtualDom_node('i');
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $author$project$Boon$toString = function (boon) {
	return function () {
		switch (boon.$) {
			case 'GainResistance':
				var resistance = boon.a;
				var bonus = boon.b;
				return $author$project$Boon$Resistance$toString(resistance) + ('+' + $elm$core$String$fromInt(bonus));
			case 'GainDomains':
				var domains = boon.a;
				var isMultiple = $elm$core$List$length(domains) > 1;
				return 'Gain the ' + (A2(
					$elm$core$String$join,
					', ',
					A2($elm$core$List$map, $author$project$Boon$Domain$toString, domains)) + (' domain' + (isMultiple ? 's' : '')));
			case 'GainSkills':
				var skills = boon.a;
				var isMultiple = $elm$core$List$length(skills) > 1;
				return 'Gain the ' + (A2(
					$elm$core$String$join,
					', ',
					A2($elm$core$List$map, $author$project$Boon$Skill$toString, skills)) + (' skill' + (isMultiple ? 's' : '')));
			case 'GainEquipment':
				var equipment = boon.a;
				return 'Gain ' + A2($elm$core$String$join, ', ', equipment);
			case 'GainRefresh':
				var refresh = boon.a;
				return 'Gain ' + (A2($elm$core$String$join, ', ', refresh) + ' as a refresh');
			case 'GainSkillKnack':
				var skill = boon.a;
				return 'Gain a Knack in the ' + ($author$project$Boon$Skill$toString(skill) + ' skill');
			default:
				var domain = boon.a;
				return 'Gain a Knack in the ' + ($author$project$Boon$Domain$toString(domain) + ' domain');
		}
	}() + '.';
};
var $author$project$Ability$view = function (ability) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$h3,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(ability.name)
					])),
				A2(
				$elm$html$Html$p,
				_List_Nil,
				_Utils_ap(
					A2(
						$elm$core$Maybe$withDefault,
						_List_Nil,
						A2(
							$elm$core$Maybe$map,
							function (flavor) {
								return _List_fromArray(
									[
										A2(
										$elm$html$Html$i,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text(flavor + ' ')
											]))
									]);
							},
							ability.flavor)),
					_Utils_ap(
						(!$elm$core$List$isEmpty(ability.boons)) ? _List_fromArray(
							[
								$elm$html$Html$text(
								A2(
									$elm$core$String$join,
									'. ',
									A2($elm$core$List$map, $author$project$Boon$toString, ability.boons)) + ' ')
							]) : _List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(ability.text)
							]))))
			]));
};
var $author$project$Abilities$viewAdvanceList = F2(
	function (selectedAbilities, abilities) {
		return A2(
			$elm$html$Html$ul,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$attribute, 'role', 'list'),
					$elm$html$Html$Attributes$class('abilities-page__list'),
					$elm$html$Html$Attributes$class('flow')
				]),
			A2(
				$elm$core$List$map,
				function (ability) {
					var isChosen = A2($elm$core$Dict$member, ability.name, selectedAbilities);
					return A2(
						$elm$html$Html$li,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$classList(
								_List_fromArray(
									[
										_Utils_Tuple2('chosen', isChosen)
									])),
								$elm$html$Html$Events$onClick(
								isChosen ? $author$project$Abilities$UnchoseAbility(ability.name) : $author$project$Abilities$ChoseAbility(ability))
							]),
						_List_fromArray(
							[
								$author$project$Ability$view(ability)
							]));
				},
				$elm$core$Dict$values(abilities)));
	});
var $author$project$Abilities$viewAdvances = F4(
	function (name, selectedAbilities, advances, isSelected) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$classList(
					_List_fromArray(
						[
							_Utils_Tuple2('hidden', !isSelected)
						]))
				]),
			function () {
				switch (advances.$) {
					case 'Loading':
						return _List_fromArray(
							[
								A2(
								$elm$html$Html$h1,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Loading ' + name)
									]))
							]);
					case 'Failed':
						return _List_fromArray(
							[
								A2(
								$elm$html$Html$h1,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Error :(')
									]))
							]);
					default:
						var low = advances.a.low;
						var medium = advances.a.medium;
						var high = advances.a.high;
						return _List_fromArray(
							[
								A2(
								$elm$html$Html$h2,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Low')
									])),
								A2($author$project$Abilities$viewAdvanceList, selectedAbilities, low),
								A2(
								$elm$html$Html$h2,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Medium')
									])),
								A2($author$project$Abilities$viewAdvanceList, selectedAbilities, medium),
								A2(
								$elm$html$Html$h2,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('High')
									])),
								A2($author$project$Abilities$viewAdvanceList, selectedAbilities, high)
							]);
				}
			}());
	});
var $author$project$Abilities$view = function (_v0) {
	var session = _v0.session;
	var selected = _v0.selected;
	var tabs = _v0.tabs;
	var chosen = _v0.chosen;
	var primary = A2(
		$elm$core$Maybe$withDefault,
		'',
		A2(
			$elm$core$Maybe$map,
			function ($) {
				return $._class;
			},
			$author$project$Session$character(session)));
	var primaryFirst = A2(
		$elm$core$Maybe$withDefault,
		$elm$core$Dict$toList(tabs),
		A2(
			$elm$core$Maybe$map,
			function (advances) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(primary, advances),
					$elm$core$Dict$toList(
						A2($elm$core$Dict$remove, primary, tabs)));
			},
			A2($elm$core$Dict$get, primary, tabs)));
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('abilities-page'),
				$elm$html$Html$Attributes$class('wrapper')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$nav,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('tab-bar')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$a,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$href(
								$author$project$Route$toString($author$project$Route$Root))
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('< Back')
							])),
						A2(
						$elm$html$Html$ul,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$attribute, 'role', 'list')
							]),
						A2(
							$elm$core$List$map,
							function (_v1) {
								var name = _v1.a;
								return A2(
									$elm$html$Html$li,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$classList(
											_List_fromArray(
												[
													_Utils_Tuple2(
													'selected',
													_Utils_eq(name, selected))
												]))
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$a,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$href(
													$author$project$Route$toString(
														$author$project$Route$Abilities(
															$elm$core$Maybe$Just(name))))
												]),
											_List_fromArray(
												[
													$elm$html$Html$text(name)
												]))
										]));
							},
							primaryFirst))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('abilities-page__advances')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_Nil,
						A2(
							$elm$core$List$map,
							function (_v2) {
								var name = _v2.a;
								var advances = _v2.b;
								return A4(
									$author$project$Abilities$viewAdvances,
									name,
									chosen,
									advances,
									_Utils_eq(name, selected));
							},
							primaryFirst)),
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('button'),
								$elm$html$Html$Events$onClick($author$project$Abilities$ApplyChosen)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Apply')
							]))
					]))
			]));
};
var $author$project$Character$UpdatedBonds = function (a) {
	return {$: 'UpdatedBonds', a: a};
};
var $author$project$Character$UpdatedDomainKnacks = function (a) {
	return {$: 'UpdatedDomainKnacks', a: a};
};
var $author$project$Character$UpdatedDomains = function (a) {
	return {$: 'UpdatedDomains', a: a};
};
var $author$project$Character$UpdatedEquipment = function (a) {
	return {$: 'UpdatedEquipment', a: a};
};
var $author$project$Character$UpdatedFallout = function (a) {
	return {$: 'UpdatedFallout', a: a};
};
var $author$project$Character$UpdatedName = function (a) {
	return {$: 'UpdatedName', a: a};
};
var $author$project$Character$UpdatedNotes = function (a) {
	return {$: 'UpdatedNotes', a: a};
};
var $author$project$Character$UpdatedRefresh = function (a) {
	return {$: 'UpdatedRefresh', a: a};
};
var $author$project$Character$UpdatedResistances = function (a) {
	return {$: 'UpdatedResistances', a: a};
};
var $author$project$Character$UpdatedSkillKnacks = function (a) {
	return {$: 'UpdatedSkillKnacks', a: a};
};
var $author$project$Character$UpdatedSkills = function (a) {
	return {$: 'UpdatedSkills', a: a};
};
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$label = _VirtualDom_node('label');
var $elm$html$Html$main_ = _VirtualDom_node('main');
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $elm$html$Html$section = _VirtualDom_node('section');
var $elm$html$Html$textarea = _VirtualDom_node('textarea');
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $elm$html$Html$Attributes$for = $elm$html$Html$Attributes$stringProperty('htmlFor');
var $author$project$TypeDict$get = F2(
	function (key, dict) {
		return A2(
			$author$project$TypeDict$delegate,
			A2(
				$elm$core$Basics$composeR,
				$elm$core$Dict$get(
					A2($author$project$TypeDict$hash, dict, key)),
				$elm$core$Maybe$map($elm$core$Tuple$second)),
			dict);
	});
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $elm$html$Html$strong = _VirtualDom_node('strong');
var $elm$core$String$toLower = _String_toLower;
var $author$project$Boon$Resistance$view = F2(
	function (toMsg, resistances) {
		var resistancesList = _List_fromArray(
			[$author$project$Boon$Resistance$Body, $author$project$Boon$Resistance$Resolve, $author$project$Boon$Resistance$Resources, $author$project$Boon$Resistance$Shadow, $author$project$Boon$Resistance$Reputation, $author$project$Boon$Resistance$Armor]);
		var labelId = A2(
			$elm$core$Basics$composeR,
			$author$project$Boon$Resistance$toString,
			A2(
				$elm$core$Basics$composeR,
				$elm$core$String$toLower,
				$elm$core$Basics$append('resistance--')));
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('resistances-table')
				]),
			_Utils_ap(
				A2(
					$elm$core$List$map,
					function (r) {
						return A2(
							$elm$html$Html$label,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$for(
									labelId(r))
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$strong,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											$author$project$Boon$Resistance$toString(r))
										]))
								]));
					},
					resistancesList),
				A2(
					$elm$core$List$map,
					function (r) {
						return A2(
							$elm$html$Html$input,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$id(
									labelId(r)),
									$elm$html$Html$Attributes$type_('number'),
									$elm$html$Html$Attributes$value(
									$elm$core$String$fromInt(
										A2(
											$elm$core$Maybe$withDefault,
											0,
											A2($author$project$TypeDict$get, r, resistances)))),
									$elm$html$Html$Events$onInput(
									function (s) {
										return toMsg(
											A3(
												$author$project$TypeDict$insert,
												r,
												A3(
													$elm$core$Basics$clamp,
													0,
													5,
													A2(
														$elm$core$Maybe$withDefault,
														0,
														$elm$core$String$toInt(s))),
												resistances));
									})
								]),
							_List_Nil);
					},
					resistancesList)));
	});
var $elm$html$Html$br = _VirtualDom_node('br');
var $elm$html$Html$details = _VirtualDom_node('details');
var $elm$html$Html$summary = _VirtualDom_node('summary');
var $author$project$Ability$viewCompact = function (ability) {
	return A2(
		$elm$html$Html$details,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('flow'),
				$elm$html$Html$Attributes$class('ability')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$summary,
				_List_Nil,
				A2(
					$elm$core$List$cons,
					A2(
						$elm$html$Html$strong,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(ability.name)
							])),
					A2(
						$elm$core$Maybe$withDefault,
						_List_Nil,
						A2(
							$elm$core$Maybe$map,
							function (flavor) {
								return _List_fromArray(
									[
										A2($elm$html$Html$br, _List_Nil, _List_Nil),
										A2(
										$elm$html$Html$i,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text(flavor + ' ')
											]))
									]);
							},
							ability.flavor)))),
				A2(
				$elm$html$Html$p,
				_List_Nil,
				_Utils_ap(
					(!$elm$core$List$isEmpty(ability.boons)) ? _List_fromArray(
						[
							$elm$html$Html$text(
							A2(
								$elm$core$String$join,
								'. ',
								A2($elm$core$List$map, $author$project$Boon$toString, ability.boons)) + ' ')
						]) : _List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(ability.text)
						])))
			]));
};
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$checked = $elm$html$Html$Attributes$boolProperty('checked');
var $elm$core$Elm$JsArray$indexedMap = _JsArray_indexedMap;
var $elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var $elm$core$Array$indexedMap = F2(
	function (func, _v0) {
		var len = _v0.a;
		var tree = _v0.c;
		var tail = _v0.d;
		var initialBuilder = {
			nodeList: _List_Nil,
			nodeListSize: 0,
			tail: A3(
				$elm$core$Elm$JsArray$indexedMap,
				func,
				$elm$core$Array$tailIndex(len),
				tail)
		};
		var helper = F2(
			function (node, builder) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldl, helper, builder, subTree);
				} else {
					var leaf = node.a;
					var offset = builder.nodeListSize * $elm$core$Array$branchFactor;
					var mappedLeaf = $elm$core$Array$Leaf(
						A3($elm$core$Elm$JsArray$indexedMap, func, offset, leaf));
					return {
						nodeList: A2($elm$core$List$cons, mappedLeaf, builder.nodeList),
						nodeListSize: builder.nodeListSize + 1,
						tail: builder.tail
					};
				}
			});
		return A2(
			$elm$core$Array$builderToArray,
			true,
			A3($elm$core$Elm$JsArray$foldl, helper, initialBuilder, tree));
	});
var $elm$html$Html$Events$targetChecked = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'checked']),
	$elm$json$Json$Decode$bool);
var $elm$html$Html$Events$onCheck = function (tagger) {
	return A2(
		$elm$html$Html$Events$on,
		'change',
		A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetChecked));
};
var $author$project$TypeDict$keys = $author$project$TypeDict$delegate(
	A2(
		$elm$core$Basics$composeR,
		$elm$core$Dict$values,
		$elm$core$List$map($elm$core$Tuple$first)));
var $author$project$TypeDict$toList = function (dict) {
	return A2(
		$elm$core$List$filterMap,
		$elm$core$Basics$identity,
		A2(
			$elm$core$List$map,
			function (key) {
				return A2(
					$elm$core$Maybe$map,
					$elm$core$Tuple$pair(key),
					A2($author$project$TypeDict$get, key, dict));
			},
			$author$project$TypeDict$keys(dict)));
};
var $elm$core$Elm$JsArray$appendN = _JsArray_appendN;
var $elm$core$Elm$JsArray$slice = _JsArray_slice;
var $elm$core$Array$appendHelpBuilder = F2(
	function (tail, builder) {
		var tailLen = $elm$core$Elm$JsArray$length(tail);
		var notAppended = ($elm$core$Array$branchFactor - $elm$core$Elm$JsArray$length(builder.tail)) - tailLen;
		var appended = A3($elm$core$Elm$JsArray$appendN, $elm$core$Array$branchFactor, builder.tail, tail);
		return (notAppended < 0) ? {
			nodeList: A2(
				$elm$core$List$cons,
				$elm$core$Array$Leaf(appended),
				builder.nodeList),
			nodeListSize: builder.nodeListSize + 1,
			tail: A3($elm$core$Elm$JsArray$slice, notAppended, tailLen, tail)
		} : ((!notAppended) ? {
			nodeList: A2(
				$elm$core$List$cons,
				$elm$core$Array$Leaf(appended),
				builder.nodeList),
			nodeListSize: builder.nodeListSize + 1,
			tail: $elm$core$Elm$JsArray$empty
		} : {nodeList: builder.nodeList, nodeListSize: builder.nodeListSize, tail: appended});
	});
var $elm$core$Array$appendHelpTree = F2(
	function (toAppend, array) {
		var len = array.a;
		var tree = array.c;
		var tail = array.d;
		var itemsToAppend = $elm$core$Elm$JsArray$length(toAppend);
		var notAppended = ($elm$core$Array$branchFactor - $elm$core$Elm$JsArray$length(tail)) - itemsToAppend;
		var appended = A3($elm$core$Elm$JsArray$appendN, $elm$core$Array$branchFactor, tail, toAppend);
		var newArray = A2($elm$core$Array$unsafeReplaceTail, appended, array);
		if (notAppended < 0) {
			var nextTail = A3($elm$core$Elm$JsArray$slice, notAppended, itemsToAppend, toAppend);
			return A2($elm$core$Array$unsafeReplaceTail, nextTail, newArray);
		} else {
			return newArray;
		}
	});
var $elm$core$Array$builderFromArray = function (_v0) {
	var len = _v0.a;
	var tree = _v0.c;
	var tail = _v0.d;
	var helper = F2(
		function (node, acc) {
			if (node.$ === 'SubTree') {
				var subTree = node.a;
				return A3($elm$core$Elm$JsArray$foldl, helper, acc, subTree);
			} else {
				return A2($elm$core$List$cons, node, acc);
			}
		});
	return {
		nodeList: A3($elm$core$Elm$JsArray$foldl, helper, _List_Nil, tree),
		nodeListSize: (len / $elm$core$Array$branchFactor) | 0,
		tail: tail
	};
};
var $elm$core$Array$append = F2(
	function (a, _v0) {
		var aTail = a.d;
		var bLen = _v0.a;
		var bTree = _v0.c;
		var bTail = _v0.d;
		if (_Utils_cmp(bLen, $elm$core$Array$branchFactor * 4) < 1) {
			var foldHelper = F2(
				function (node, array) {
					if (node.$ === 'SubTree') {
						var tree = node.a;
						return A3($elm$core$Elm$JsArray$foldl, foldHelper, array, tree);
					} else {
						var leaf = node.a;
						return A2($elm$core$Array$appendHelpTree, leaf, array);
					}
				});
			return A2(
				$elm$core$Array$appendHelpTree,
				bTail,
				A3($elm$core$Elm$JsArray$foldl, foldHelper, a, bTree));
		} else {
			var foldHelper = F2(
				function (node, builder) {
					if (node.$ === 'SubTree') {
						var tree = node.a;
						return A3($elm$core$Elm$JsArray$foldl, foldHelper, builder, tree);
					} else {
						var leaf = node.a;
						return A2($elm$core$Array$appendHelpBuilder, leaf, builder);
					}
				});
			return A2(
				$elm$core$Array$builderToArray,
				true,
				A2(
					$elm$core$Array$appendHelpBuilder,
					bTail,
					A3(
						$elm$core$Elm$JsArray$foldl,
						foldHelper,
						$elm$core$Array$builderFromArray(a),
						bTree)));
		}
	});
var $elm$core$Array$length = function (_v0) {
	var len = _v0.a;
	return len;
};
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $elm$core$Array$sliceLeft = F2(
	function (from, array) {
		var len = array.a;
		var tree = array.c;
		var tail = array.d;
		if (!from) {
			return array;
		} else {
			if (_Utils_cmp(
				from,
				$elm$core$Array$tailIndex(len)) > -1) {
				return A4(
					$elm$core$Array$Array_elm_builtin,
					len - from,
					$elm$core$Array$shiftStep,
					$elm$core$Elm$JsArray$empty,
					A3(
						$elm$core$Elm$JsArray$slice,
						from - $elm$core$Array$tailIndex(len),
						$elm$core$Elm$JsArray$length(tail),
						tail));
			} else {
				var skipNodes = (from / $elm$core$Array$branchFactor) | 0;
				var helper = F2(
					function (node, acc) {
						if (node.$ === 'SubTree') {
							var subTree = node.a;
							return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
						} else {
							var leaf = node.a;
							return A2($elm$core$List$cons, leaf, acc);
						}
					});
				var leafNodes = A3(
					$elm$core$Elm$JsArray$foldr,
					helper,
					_List_fromArray(
						[tail]),
					tree);
				var nodesToInsert = A2($elm$core$List$drop, skipNodes, leafNodes);
				if (!nodesToInsert.b) {
					return $elm$core$Array$empty;
				} else {
					var head = nodesToInsert.a;
					var rest = nodesToInsert.b;
					var firstSlice = from - (skipNodes * $elm$core$Array$branchFactor);
					var initialBuilder = {
						nodeList: _List_Nil,
						nodeListSize: 0,
						tail: A3(
							$elm$core$Elm$JsArray$slice,
							firstSlice,
							$elm$core$Elm$JsArray$length(head),
							head)
					};
					return A2(
						$elm$core$Array$builderToArray,
						true,
						A3($elm$core$List$foldl, $elm$core$Array$appendHelpBuilder, initialBuilder, rest));
				}
			}
		}
	});
var $elm$core$Array$fetchNewTail = F4(
	function (shift, end, treeEnd, tree) {
		fetchNewTail:
		while (true) {
			var pos = $elm$core$Array$bitMask & (treeEnd >>> shift);
			var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (_v0.$ === 'SubTree') {
				var sub = _v0.a;
				var $temp$shift = shift - $elm$core$Array$shiftStep,
					$temp$end = end,
					$temp$treeEnd = treeEnd,
					$temp$tree = sub;
				shift = $temp$shift;
				end = $temp$end;
				treeEnd = $temp$treeEnd;
				tree = $temp$tree;
				continue fetchNewTail;
			} else {
				var values = _v0.a;
				return A3($elm$core$Elm$JsArray$slice, 0, $elm$core$Array$bitMask & end, values);
			}
		}
	});
var $elm$core$Array$hoistTree = F3(
	function (oldShift, newShift, tree) {
		hoistTree:
		while (true) {
			if ((_Utils_cmp(oldShift, newShift) < 1) || (!$elm$core$Elm$JsArray$length(tree))) {
				return tree;
			} else {
				var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, 0, tree);
				if (_v0.$ === 'SubTree') {
					var sub = _v0.a;
					var $temp$oldShift = oldShift - $elm$core$Array$shiftStep,
						$temp$newShift = newShift,
						$temp$tree = sub;
					oldShift = $temp$oldShift;
					newShift = $temp$newShift;
					tree = $temp$tree;
					continue hoistTree;
				} else {
					return tree;
				}
			}
		}
	});
var $elm$core$Array$sliceTree = F3(
	function (shift, endIdx, tree) {
		var lastPos = $elm$core$Array$bitMask & (endIdx >>> shift);
		var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, lastPos, tree);
		if (_v0.$ === 'SubTree') {
			var sub = _v0.a;
			var newSub = A3($elm$core$Array$sliceTree, shift - $elm$core$Array$shiftStep, endIdx, sub);
			return (!$elm$core$Elm$JsArray$length(newSub)) ? A3($elm$core$Elm$JsArray$slice, 0, lastPos, tree) : A3(
				$elm$core$Elm$JsArray$unsafeSet,
				lastPos,
				$elm$core$Array$SubTree(newSub),
				A3($elm$core$Elm$JsArray$slice, 0, lastPos + 1, tree));
		} else {
			return A3($elm$core$Elm$JsArray$slice, 0, lastPos, tree);
		}
	});
var $elm$core$Array$sliceRight = F2(
	function (end, array) {
		var len = array.a;
		var startShift = array.b;
		var tree = array.c;
		var tail = array.d;
		if (_Utils_eq(end, len)) {
			return array;
		} else {
			if (_Utils_cmp(
				end,
				$elm$core$Array$tailIndex(len)) > -1) {
				return A4(
					$elm$core$Array$Array_elm_builtin,
					end,
					startShift,
					tree,
					A3($elm$core$Elm$JsArray$slice, 0, $elm$core$Array$bitMask & end, tail));
			} else {
				var endIdx = $elm$core$Array$tailIndex(end);
				var depth = $elm$core$Basics$floor(
					A2(
						$elm$core$Basics$logBase,
						$elm$core$Array$branchFactor,
						A2($elm$core$Basics$max, 1, endIdx - 1)));
				var newShift = A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep);
				return A4(
					$elm$core$Array$Array_elm_builtin,
					end,
					newShift,
					A3(
						$elm$core$Array$hoistTree,
						startShift,
						newShift,
						A3($elm$core$Array$sliceTree, startShift, endIdx, tree)),
					A4($elm$core$Array$fetchNewTail, startShift, end, endIdx, tree));
			}
		}
	});
var $elm$core$Array$translateIndex = F2(
	function (index, _v0) {
		var len = _v0.a;
		var posIndex = (index < 0) ? (len + index) : index;
		return (posIndex < 0) ? 0 : ((_Utils_cmp(posIndex, len) > 0) ? len : posIndex);
	});
var $elm$core$Array$slice = F3(
	function (from, to, array) {
		var correctTo = A2($elm$core$Array$translateIndex, to, array);
		var correctFrom = A2($elm$core$Array$translateIndex, from, array);
		return (_Utils_cmp(correctFrom, correctTo) > 0) ? $elm$core$Array$empty : A2(
			$elm$core$Array$sliceLeft,
			correctFrom,
			A2($elm$core$Array$sliceRight, correctTo, array));
	});
var $author$project$Boon$Knack$remove = F2(
	function (key, idx) {
		return A2(
			$author$project$TypeDict$update,
			key,
			$elm$core$Maybe$map(
				function (knacks) {
					var start = A3($elm$core$Array$slice, 0, idx, knacks);
					var end = A3(
						$elm$core$Array$slice,
						idx + 1,
						$elm$core$Array$length(knacks),
						knacks);
					return A2($elm$core$Array$append, start, end);
				}));
	});
var $elm$core$Array$setHelp = F4(
	function (shift, index, value, tree) {
		var pos = $elm$core$Array$bitMask & (index >>> shift);
		var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
		if (_v0.$ === 'SubTree') {
			var subTree = _v0.a;
			var newSub = A4($elm$core$Array$setHelp, shift - $elm$core$Array$shiftStep, index, value, subTree);
			return A3(
				$elm$core$Elm$JsArray$unsafeSet,
				pos,
				$elm$core$Array$SubTree(newSub),
				tree);
		} else {
			var values = _v0.a;
			var newLeaf = A3($elm$core$Elm$JsArray$unsafeSet, $elm$core$Array$bitMask & index, value, values);
			return A3(
				$elm$core$Elm$JsArray$unsafeSet,
				pos,
				$elm$core$Array$Leaf(newLeaf),
				tree);
		}
	});
var $elm$core$Array$set = F3(
	function (index, value, array) {
		var len = array.a;
		var startShift = array.b;
		var tree = array.c;
		var tail = array.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? array : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			tree,
			A3($elm$core$Elm$JsArray$unsafeSet, $elm$core$Array$bitMask & index, value, tail)) : A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			A4($elm$core$Array$setHelp, startShift, index, value, tree),
			tail));
	});
var $author$project$Boon$Knack$update = F3(
	function (key, idx, val) {
		return $elm$core$String$isEmpty(val) ? A2($author$project$Boon$Knack$remove, key, idx) : A2(
			$author$project$TypeDict$update,
			key,
			$elm$core$Maybe$map(
				A2($elm$core$Array$set, idx, val)));
	});
var $author$project$Character$viewTraitList = F4(
	function (toMsg, knacksToMsg, dict, knacks) {
		var list = $author$project$TypeDict$toList(dict);
		var labeller = $author$project$TypeDict$toHasher(dict);
		var labelId = A2(
			$elm$core$Basics$composeR,
			labeller,
			A2(
				$elm$core$Basics$composeR,
				$elm$core$String$toLower,
				$elm$core$Basics$append('cb-')));
		var viewItem = function (_v1) {
			var k = _v1.a;
			var isChecked = _v1.b;
			return A2(
				$elm$html$Html$li,
				_List_Nil,
				A2(
					$elm$core$List$cons,
					A2(
						$elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$input,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$id(
										labelId(k)),
										$elm$html$Html$Attributes$type_('checkbox'),
										$elm$html$Html$Attributes$checked(isChecked),
										$elm$html$Html$Events$onCheck(
										function (b) {
											return toMsg(
												A3($author$project$TypeDict$insert, k, b, dict));
										})
									]),
								_List_Nil),
								A2(
								$elm$html$Html$label,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$for(
										labelId(k))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(
										labeller(k))
									])),
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('add-knack-button'),
										$elm$html$Html$Events$onClick(
										knacksToMsg(
											A3($author$project$Boon$Knack$insert, k, '', knacks)))
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$strong,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('+')
											]))
									]))
							])),
					function () {
						var _v0 = A2($author$project$TypeDict$get, k, knacks);
						if (_v0.$ === 'Just') {
							var ks = _v0.a;
							return _List_fromArray(
								[
									A2(
									$elm$html$Html$ul,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('knacks__list')
										]),
									$elm$core$Array$toList(
										A2(
											$elm$core$Array$indexedMap,
											F2(
												function (idx, knack) {
													return A2(
														$elm$html$Html$li,
														_List_Nil,
														_List_fromArray(
															[
																A2(
																$elm$html$Html$input,
																_List_fromArray(
																	[
																		$elm$html$Html$Attributes$value(knack),
																		$elm$html$Html$Events$onInput(
																		function (newKnack) {
																			return knacksToMsg(
																				A4($author$project$Boon$Knack$update, k, idx, newKnack, knacks));
																		})
																	]),
																_List_Nil)
															]));
												}),
											ks)))
								]);
						} else {
							return _List_Nil;
						}
					}()));
		};
		return A2(
			$elm$html$Html$ul,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$attribute, 'role', 'list'),
					$elm$html$Html$Attributes$class('checkbox-list')
				]),
			A2($elm$core$List$map, viewItem, list));
	});
var $author$project$Character$view = function (character) {
	return {
		body: _List_fromArray(
			[
				A2(
				$elm$html$Html$main_,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('wrapper gap-top-700'),
						$elm$html$Html$Attributes$class('character')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$section,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('name')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$label,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$h1,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('Name')
											])),
										A2(
										$elm$html$Html$input,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$type_('text'),
												$elm$html$Html$Attributes$value(character.name),
												$elm$html$Html$Events$onInput($author$project$Character$UpdatedName)
											]),
										_List_Nil)
									]))
							])),
						A2(
						$elm$html$Html$section,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('class')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$h2,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Class')
									])),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('text-500')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(character._class)
									]))
							])),
						A2(
						$elm$html$Html$section,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('assignment')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$h2,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Assignment')
									])),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('text-500')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(character.assignment)
									]))
							])),
						A2(
						$elm$html$Html$section,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('resistances')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$h2,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Resistances')
									])),
								A2($author$project$Boon$Resistance$view, $author$project$Character$UpdatedResistances, character.resistances)
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('skills-and-domains')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$section,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('skills')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$h2,
										_List_Nil,
										_List_fromArray(
											[
												A2(
												$elm$html$Html$a,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$href(
														$author$project$Route$toString(
															A2($author$project$Route$PlayAid, $author$project$PlayAids$Skills, $elm$core$Maybe$Nothing)))
													]),
												_List_fromArray(
													[
														$elm$html$Html$text('Skills')
													]))
											])),
										A4($author$project$Character$viewTraitList, $author$project$Character$UpdatedSkills, $author$project$Character$UpdatedSkillKnacks, character.skills, character.skillKnacks)
									])),
								A2(
								$elm$html$Html$section,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('domains')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$h2,
										_List_Nil,
										_List_fromArray(
											[
												A2(
												$elm$html$Html$a,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$href(
														$author$project$Route$toString(
															A2($author$project$Route$PlayAid, $author$project$PlayAids$Domains, $elm$core$Maybe$Nothing)))
													]),
												_List_fromArray(
													[
														$elm$html$Html$text('Domains')
													]))
											])),
										A4($author$project$Character$viewTraitList, $author$project$Character$UpdatedDomains, $author$project$Character$UpdatedDomainKnacks, character.domains, character.domainKnacks)
									]))
							])),
						A2(
						$elm$html$Html$section,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('flow'),
								$elm$html$Html$Attributes$class('abilities')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$h2,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$a,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$href(
												$author$project$Route$toString(
													$author$project$Route$Abilities(
														$elm$core$Maybe$Just(character._class))))
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('Abilities')
											]))
									])),
								A2(
								$elm$html$Html$ul,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('flow gap-top-500'),
										$elm$html$Html$Attributes$class('abilities-list'),
										A2($elm$html$Html$Attributes$attribute, 'role', 'list')
									]),
								A2(
									$elm$core$List$map,
									function (ability) {
										return A2(
											$elm$html$Html$li,
											_List_Nil,
											_List_fromArray(
												[
													$author$project$Ability$viewCompact(ability)
												]));
									},
									$elm$core$Dict$values(character.abilities)))
							])),
						A2(
						$elm$html$Html$section,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('fallout')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$h2,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Fallout')
									])),
								A2(
								$elm$html$Html$textarea,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('textbox'),
										$elm$html$Html$Events$onInput($author$project$Character$UpdatedFallout)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(character.fallout)
									]))
							])),
						A2(
						$elm$html$Html$section,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('equipment')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$h2,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$a,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$href(
												$author$project$Route$toString(
													A2($author$project$Route$PlayAid, $author$project$PlayAids$Weapons, $elm$core$Maybe$Nothing)))
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('Equipment')
											]))
									])),
								A2(
								$elm$html$Html$textarea,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('textbox'),
										$elm$html$Html$Events$onInput($author$project$Character$UpdatedEquipment)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(character.equipment)
									]))
							])),
						A2(
						$elm$html$Html$section,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('refresh')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$h2,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Refresh')
									])),
								A2(
								$elm$html$Html$textarea,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('textbox'),
										$elm$html$Html$Events$onInput($author$project$Character$UpdatedRefresh)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(character.refresh)
									]))
							])),
						A2(
						$elm$html$Html$section,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('bonds')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$h2,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Bonds')
									])),
								A2(
								$elm$html$Html$textarea,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('textbox'),
										$elm$html$Html$Events$onInput($author$project$Character$UpdatedBonds)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(character.bonds)
									]))
							])),
						A2(
						$elm$html$Html$section,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('notes')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$h2,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Notes')
									])),
								A2(
								$elm$html$Html$textarea,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('textbox'),
										$elm$html$Html$Events$onInput($author$project$Character$UpdatedNotes)
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(character.notes)
									]))
							]))
					]))
			]),
		title: character.name
	};
};
var $elm$url$Url$Builder$absolute = F2(
	function (pathSegments, parameters) {
		return '/' + (A2($elm$core$String$join, '/', pathSegments) + $elm$url$Url$Builder$toQuery(parameters));
	});
var $elm$html$Html$article = _VirtualDom_node('article');
var $elm$html$Html$b = _VirtualDom_node('b');
var $elm$html$Html$dd = _VirtualDom_node('dd');
var $elm$html$Html$dl = _VirtualDom_node('dl');
var $elm$html$Html$dt = _VirtualDom_node('dt');
var $elm$core$String$toUpper = _String_toUpper;
var $author$project$PlayAids$topicToStringPretty = function (topic) {
	var stringified = $author$project$PlayAids$topicToString(topic);
	return _Utils_ap(
		$elm$core$String$toUpper(
			A2($elm$core$String$left, 1, stringified)),
		A2($elm$core$String$dropLeft, 1, stringified));
};
var $author$project$PlayAids$view = function (playAid) {
	var stringifiedTopic = $author$project$PlayAids$topicToStringPretty(playAid.topic);
	return {
		body: _List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('wrapper')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$nav,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('tab-bar')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$a,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$href('/reves/')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('< Back')
									])),
								A2(
								$elm$html$Html$ul,
								_List_fromArray(
									[
										A2($elm$html$Html$Attributes$attribute, 'role', 'list')
									]),
								A2(
									$elm$core$List$map,
									function (topic) {
										return A2(
											$elm$html$Html$li,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$classList(
													_List_fromArray(
														[
															_Utils_Tuple2(
															'selected',
															_Utils_eq(topic, playAid.topic))
														]))
												]),
											_List_fromArray(
												[
													A2(
													$elm$html$Html$a,
													_List_fromArray(
														[
															$elm$html$Html$Attributes$href(
															A2(
																$elm$url$Url$Builder$absolute,
																_List_fromArray(
																	[
																		'reves',
																		'play-aid',
																		$author$project$PlayAids$topicToString(topic)
																	]),
																_List_Nil))
														]),
													_List_fromArray(
														[
															$elm$html$Html$text(
															$author$project$PlayAids$topicToStringPretty(topic))
														]))
												]));
									},
									_List_fromArray(
										[$author$project$PlayAids$Weapons, $author$project$PlayAids$Armor, $author$project$PlayAids$Skills, $author$project$PlayAids$Domains])))
							])),
						A2(
						$elm$html$Html$article,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('play-aid')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$h1,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(stringifiedTopic)
									])),
								function () {
								var _v0 = playAid.aids;
								switch (_v0.$) {
									case 'Failed':
										return $elm$html$Html$text('Error :(');
									case 'Loading':
										return $elm$html$Html$text('Loading...');
									default:
										var aids = _v0.a;
										return A2(
											$elm$html$Html$dl,
											_List_Nil,
											$elm$core$List$concat(
												A2(
													$elm$core$List$map,
													function (_v1) {
														var name = _v1.a;
														var rules = _v1.b;
														return _List_fromArray(
															[
																A2(
																$elm$html$Html$dt,
																_List_fromArray(
																	[
																		$elm$html$Html$Attributes$id(name)
																	]),
																_List_fromArray(
																	[
																		A2(
																		$elm$html$Html$b,
																		_List_Nil,
																		_List_fromArray(
																			[
																				$elm$html$Html$text(name)
																			]))
																	])),
																A2(
																$elm$html$Html$dd,
																_List_Nil,
																_List_fromArray(
																	[
																		$elm$html$Html$text(rules)
																	]))
															]);
													},
													$elm$core$Dict$toList(aids))));
								}
							}()
							]))
					]))
			]),
		title: stringifiedTopic
	};
};
var $author$project$Main$view = function (model) {
	switch (model.$) {
		case 'DecodeErr':
			var err = model.b;
			return {
				body: _List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('wrapper gap-top-700 flow')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$h1,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Something went wrong :(')
									])),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(
										$elm$json$Json$Decode$errorToString(err))
									])),
								A2(
								$elm$html$Html$p,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$elm$html$Html$Events$onClick($author$project$Main$ClickedOpenFile)
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('Try a different file')
											]))
									]))
							]))
					]),
				title: 'Error'
			};
		case 'Landing':
			return {
				body: _List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('wrapper gap-top-700 flow')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$h1,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Welcome')
									])),
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$elm$html$Html$Events$onClick($author$project$Main$ClickedOpenFile)
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('Open a character')
											])),
										$elm$html$Html$text(' or '),
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$elm$html$Html$Events$onClick($author$project$Main$ClickedNewCharacter)
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('Create a new one?')
											]))
									]))
							]))
					]),
				title: 'Welcome!'
			};
		case 'PickClass':
			return {
				body: _List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('class-picker'),
								$elm$html$Html$Attributes$class('wrapper gap-top-700 flow')
							]),
						A2(
							$elm$core$List$cons,
							A2(
								$elm$html$Html$h1,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Pick your class')
									])),
							A2(
								$elm$core$List$map,
								function (class_) {
									return A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$elm$html$Html$Events$onClick(
												$author$project$Main$PickedClass(class_))
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(class_.name)
											]));
								},
								$author$project$Class$classes)))
					]),
				title: 'Pick a class'
			};
		case 'PickAssignment':
			return {
				body: _List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('assignment-picker'),
								$elm$html$Html$Attributes$class('wrapper gap-top-700 flow')
							]),
						A2(
							$elm$core$List$cons,
							A2(
								$elm$html$Html$h1,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Pick your assignment')
									])),
							A2(
								$elm$core$List$map,
								function (assignment) {
									return A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$elm$html$Html$Events$onClick(
												$author$project$Main$PickedAssignment(assignment))
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(assignment.name)
											]));
								},
								$author$project$Boon$assignments)))
					]),
				title: 'Pick an assignment'
			};
		case 'Character':
			var session = model.a;
			var _v1 = $author$project$Session$character(session);
			if (_v1.$ === 'Just') {
				var character = _v1.a;
				var characterView = $author$project$Character$view(character);
				return {
					body: _Utils_ap(
						A2(
							$elm$core$List$map,
							$elm$html$Html$map($author$project$Main$CharacterMsg),
							characterView.body),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$footer,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('footer'),
										$elm$html$Html$Attributes$class('bg-light-shade')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$span,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text(
												$author$project$Session$changesToString(
													$author$project$Session$changes(session)))
											])),
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$elm$html$Html$Events$onClick($author$project$Main$ClickedSave),
												$elm$html$Html$Attributes$class('button'),
												$elm$html$Html$Attributes$classList(
												_List_fromArray(
													[
														_Utils_Tuple2(
														'unsaved',
														!$author$project$Session$isSaved(session))
													]))
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('Save')
											]))
									]))
							])),
					title: characterView.title
				};
			} else {
				return {
					body: _List_fromArray(
						[
							A2(
							$elm$html$Html$p,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('Looks like you don\'t have a character yet. Wanna '),
									A2(
									$elm$html$Html$a,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$href('/reves/')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('make one')
										])),
									$elm$html$Html$text('?')
								]))
						]),
					title: 'New Character'
				};
			}
		case 'Abilities':
			var abilities = model.a;
			return {
				body: _List_fromArray(
					[
						A2(
						$elm$html$Html$map,
						$author$project$Main$AbilitiesMsg,
						$author$project$Abilities$view(abilities))
					]),
				title: 'Abilities / ' + abilities.selected
			};
		default:
			var playAid = model.b;
			return $author$project$PlayAids$view(playAid);
	}
};
var $author$project$Main$main = $elm$browser$Browser$application(
	{init: $author$project$Main$init, onUrlChange: $author$project$Main$UrlChanged, onUrlRequest: $author$project$Main$LinkClicked, subscriptions: $author$project$Main$subscriptions, update: $author$project$Main$update, view: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				$elm$json$Json$Decode$null($elm$core$Maybe$Nothing),
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, $elm$json$Json$Decode$string)
			])))(0)}});}(this));