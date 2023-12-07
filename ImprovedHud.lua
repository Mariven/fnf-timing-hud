---@diagnostic disable: lowercase-global

--[[ Notes: 
-- https://github.com/ShadowMario/FNF-PsychEngine/wiki/Lua-Script-API:-Tweens-and-Timers
-- list of tweens: https://api.haxeflixel.com/flixel/tweens/FlxEase.html
-- advanced API functions: https://gamebanana.com/tuts/15333
-- keycodes: https://gamebanana.com/tuts/15319
-- flixel key list: https://api.haxeflixel.com/flixel/input/keyboard/FlxKeyList.html
-- symbol key for wingdings 1/2/3 and webdings: https://i.imgur.com/ZK0qRsZ.png
 * use camelCase (exceptions made for acronyms and functionals where appropriate)
 * only add trivial lines (ends, single brackets, etc.) where they add clarity after long blocks of code
 * break lines so they don't extend the horizontal scroll bar too far
]]


--**********************************************************--
--                     OPTIONS                              --
--**********************************************************--

--indents indicate hierarchy of containment
--the first bool in each sub is an override in case the super is disabled (false by default)
--the second is a specifier in case the super is enabled (true by default)
cornerTextEnabled = false
horizontalMeterEnabled = true
	horizontalBarEnabled = false or (horizontalMeterEnabled and false)
		averageMarkerEnabled = true or (horizontalBarEnabled and true)
		perfectRangeEnabled = true or (horizontalBarEnabled and true)
		horizontalPositionEnabled = true or (horizontalBarEnabled and true)
		warningLightsEnabled = false or (horizontalBarEnabled and false) -- blocked
msDisplayEnabled = true
verticalMeterEnabled = true
diamondsEnabled = true
	pMainDiamondEnabled = false or (diamondsEnabled and true)
		pDiamondButtonsEnabled = false or (pMainDiamondEnabled and false)
		pDiamondBGEnabled = false or (pMainDiamondEnabled and false) -- blocked
		pDiamondProxyEnabled = false or (pMainDiamondEnabled and true)
		pDiamondArrowsEnabled = false or (pMainDiamondEnabled and true)
		pDiamondRingEnabled = false or (pMainDiamondEnabled and true)
		pDiamondRing2Enabled = false or (pMainDiamondEnabled and true) -- the yellow circle
	eMainDiamondEnabled = false or (diamondsEnabled and true)
		eDiamondButtonsEnabled = false or (eMainDiamondEnabled and false)
		eDiamondBGEnabled = false or (eMainDiamondEnabled and false) -- blocked
		eDiamondProxyEnabled = false or (eMainDiamondEnabled and true)
		eDiamondArrowsEnabled = false or (eMainDiamondEnabled and true)

histogramEnabled = true
histogramBars = 31
kernel = {1, 2, 4, 2, 1}


--**********************************************************--
--                 CUSTOMN'T VARIABLES                      --
--         (can make these whatever you want)               --
--     (so long as you make them the right thing)           --
--**********************************************************--
arrowKeys = {left="D", down="F", up="J", right="K"}


--**********************************************************--
--                 PARAMETER VARIABLES                      --
--      (not necessary to change these, but you can)        --
--**********************************************************--
colors =
{	red="ff0040", orange="ffb500", brightGold="fff65e", gold="e8b923", 
	lightGreen = "59ff24", green="11dd11", cyan="5ce8e3", blue="0000ff",
	purple="ac00e6", magenta="c749b7", lightGray="9bb4b8", white="ffffff", black="000000"} 
moveRate=10; growRate=0.03
keyList = { -- we loop over this list every frame, so use only what is needed (idk how much it actually helps, though)
	--[[backtick="GRAVE",]]
	one="ONE",two="TWO",three="THREE",
	--[[four="FOUR",]]--[[five="FIVE",]]--[[six="SIX",]]--[[seven="SEVEN",]]--[[eight="EIGHT",]]--[[nine="NINE",]]
	--[[zero="ZERO",]]--[[minus="MINUS",]]--[[equals="PLUS",]]--[[backspace="BACKSPACE",]]--[[tab="TAB",]]
	q="Q",w="W",e="E",
	--[[r="R",]]--[[t="T",]]--[[y="Y",]]--[[u="U",]]--[[i="I",]]--[[o="O",]]--[[p="P",]]
	--[[leftBracket="LBRACKET",]]--[[rightBracket="RBRACKET",]]--[[backslash="BACKSLASH",]]--[[caps="CAPSLOCK",]]
	--[[a="A",]]--[[s="S",]]--[[d="D",]]--[[f="F",]]--[[g="G",]]--[[h="H",]]--[[j="J",]]--[[k="K",]]--[[l="L",]]
	--[[semicolon="SEMICOLON",quote="QUOTE",enter="ENTER",]]
	shift="SHIFT",
	--[[z="Z",]]--[[x="X",]]--[[c="C",]]--[[v="V",]]--[[b="B",]]--[[n="N",]]--[[m="M",]]
	--[[comma="COMMA",]]--[[period="PERIOD",]]--[[slash="SLASH",]]
	ctrl="CONTROL",
	--[[alt="ALT",]]
	space="SPACE",leftArrow="LEFT",rightArrow="RIGHT",upArrow="UP",downArrow="DOWN",
	left=arrowKeys.left, right=arrowKeys.right, up=arrowKeys.up, down=arrowKeys.down}
luaDebugMode=true --for debugPrint

-- UTILITY VARIABLES (should not change these unless necessary for code changes)
directions = {left = 'singLEFT', down = 'singDOWN', up = 'singUP', right = 'singRIGHT', space = 'hey',
leftArrow = 'singLEFT', rightArrow = 'singRIGHT', upArrow = 'singUP', downArrow = 'singDOWN'}
capitals = {left = "Left", down = "Down", up = "Up", right = "Right", space = "Space"} -- lol
pDiamondArrowTags = {left = "pDiamondLeftArrow", down = "pDiamondDownArrow", up = "pDiamondUpArrow", right = "pDiamondRightArrow"}
pDiamondProxyTags = {left = "pDiamondLeftProxy", down = "pDiamondDownProxy", up = "pDiamondUpProxy", right = "pDiamondRightProxy"}
keysPressed = {}
keysJustPressed = {}
keysJustReleased = {}
tracker={}; avgCalc={}; char = {}; index = 1
for k, v in pairs(keyList) do keysPressed[k]=false; keysJustPressed[k]=false; keysJustReleased[k]=false end
keyMovements={up={0,-moveRate},down={0,moveRate},left={-moveRate,0},right={moveRate,0},
				upArrow={0,-moveRate},downArrow={0,moveRate},leftArrow={-moveRate,0},rightArrow={moveRate,0}}
Cap321 = {"three", "two", "one"}
noteDirections = {{"Left", "l"}, {"Down", "d"}, {"Up", "u"}, {"Right", "r"}}
ringBarList = {}
TU = 0.5 -- length of a quarter note, updated automatically as 60/curBpm
doneUpdate = 0
lastBeatMain = false
if opponentPlay
	then opponentDiamondPrefix='p'; playerDiamondPrefix='e'
	else opponentDiamondPrefix='e';playerDiamondPrefix='p' end

-- opponent play is stored in variable opponentPlay


function Y(f) return f(f) end -- inaugurate the script with a Y combinator
function isTable(T) return type(T) == type(table) end
function isNotTable(T) return type(T) ~= type(table) end
function isString(s) return type(s) == type('') end
function isNotString(s) return type(s) ~= type('') end
function isFunction(f) return type(f) == type(isFunction) end
function isNotFunction(f) return type(f) ~= type(isNotFunction) end
function isUnderscores(s) return (#string.gsub(s,'_','') == 0) end
function isSpecial(s) return (s=='parent' or s=='previous' or s=='properties') end
function isProtected(s) return (isUnderscores(s) or isSpecial(s)) end
function hasKey(T, x) return T[x] ~= nil end -- it's a neurosis, ok?
function hasNoKey(T, x) return T[x] == nil end
function guaranteeKeysExist(T, keys, fluff)
	-- guarantees that a given table `T` has non-nil variables for
	-- a given list of keys, by equipping it with a vacuous but non-nil
	-- fluff value (e.g. "foo", 0, {}) for all the keys it's missing
	fluff = fluff or {} -- original use is guaranteeing existence of generics, so {} default
	for n, key in ipairs(keys) do
		if hasNoKey(T, key) then T[key] = fluff
end end end

function schematicType(s, short)
	-- get the schematic type of an object from its name; if not short, given in table format
	if short ~= true then
		if s == '_'
			then return {generic = true, concrete = true, family = false, object = false, property = false}
		elseif s == '__'
			then return {generic = true, concrete = false, family = true, object = false, property = false}
		elseif (s:byte(1) == 95) and (s:byte(2) == 95)
			then return {generic = false, concrete = false, family = true, object = true, property = false}
		elseif s:byte(1) == 95
			then return {generic = false, concrete = true, family = false, object = true, property = false}
		elseif true
			then return {generic = false, concrete = false, family = false, object = false, property = true} end
	else
		if s == '_' then return "concrete generic"
		elseif s == '__' then return "family generic"
		elseif s:byte(1) == 95 then
			if s:byte(2) == 95 then return "family object"
			else return "concrete object" end
		else return "property" end
end end

function RGBtoHex(r,g,b)
	lol = {"0","1","2","3","4","5","6","7","8","9","A","B","C","D","E","F"}
	if r>255 then r=255 elseif r<0 then r=0 end
	if g>255 then g=255 elseif g<0 then g=0 end
	if b>255 then b=255 elseif b<0 then b=0 end
	return lol[math.floor(r/16)+1]..lol[math.floor(r%16)+1]..lol[math.floor(g/16)+1]
	..lol[math.floor(g%16)+1]..lol[math.floor(b/16)+1]..lol[math.floor(b%16)+1] end

function HextoRGB(h)
	return tonumber(h:sub(1,2),16), tonumber(h:sub(3,4),16), tonumber(h:sub(5,6),16) end

function RGBtoHSV(r,g,b)
	local max=math.max(r,g,b); local min=math.min(r,g,b)
	if max==min then return 0, 0, max end
	if r==max then return ((g<b) and 1 or 0)+((g-b)/(max-min))/6, (max-min)/max, max end
	if g==max then return ((b-r)/(max-min)+2)/6, (max-min)/max, max end
	return ((r-g)/(max-min)+4)/6, (max-min)/max, max end

function HSVtoRGB(h,s,v)
	local i=math.floor(h*6)
	magic={{2,3,4},{1,2,4},{4,2,3},{4,1,2},{3,4,2},{2,4,1}}
	S={v*(1-(h*6-i)*s), v, v*(1-(1-(h*6-i))*s), v*(1-s)}
	return S[magic[i%6+1][1]]*255, S[magic[i%6+1][2]]*255, S[magic[i%6+1][3]]*255 end

Map = function(f) return function(T)
		TT = {}
		for k, v in ipairs(T) do TT[k] = f(v) end 
		return(TT) end end

Eq = function(x,y) return x==y end -- equality predicate

Wrap = function(f) -- Wrap(f)({a, b, c}) = f(a, b, c)
	return function(x) return f(table.unwrap(x)) end end

Unwrap = function(f) -- Unwrap(f)(a, b, c) = f({a, b, c})
	return function(...) return f({...}) end end

Comp = function(f,g) -- Comp(f, g)(a, b) = f(g(a, b))
	return function(x) return f(g(x)) end end

Id = function(x) return x end -- Id(a) = a

Curry = function(f, args) -- Curry(f, a, b)(c, d) = f(a, b, c, d)
	return function(...) return f(table.unwrap(table.append(args, {...}))) end end

Permute = function(f, permutation) -- e.g. Permute(f, {2, 1, 3})(a, b, c) = f(b, a, c)
	return function(...)
		local args = {...}; local newargs = {}
		for k, v in ipairs(permutation) do newargs[k] = args[v] end
		return f(table.unpack(newargs)) end end

Apply = function(...) -- Apply(a, b, c)(f) = f(a, b, c)
	local args = {...}
	return function(f) return f(table.unpack({args})) end end

Curry2 = function(f, n, N)
	-- f is multivariate function, n is array of indices to be curried,
	-- N is total number of variables
	-- e.g. , if f takes five arguments a...e, then
	-- 	Curry(f, {1, 2,4}, 5) is the function f' that takes (a, b, c) to
	-- 	the function f'' that takes (d, e) to (a, b, d, c, e)
	if n == nil
		then n = {1} end
	if not(type(n)==type(table))
		then n = {n} end
	return function(...)
		T = {}; TT = {};
		for i,v in ipairs(n) do T[v] = arg[i] end
		i = 1; j = 1; nn = {}
		while i <= N do
			if T[i] == nil then nn[j] = i; j = j+1 end
			i = i+1
		end
		return function(...)
			for i, v in ipairs(nn) do
				T[v] = arg[i] end
			return(f(unpack(T)))
	end end end

-- prop ? val_true : val_false
function ifElse(prop, val_true, val_false)
	if prop then return val_true 
	else return val_false 
end end

function ifThen(prop, val_true, val_false) --screwed this up a couple times
	if prop then return val_true 
	else return val_false 
end end

function foldOverTable(base, func, table)
	-- func is a function from baseType x keyType x valueType to baseType;
	-- it literally just is a fold over a table (let's hope for no order weirdness)
	-- if func just takes in the values, simply pass `function(x,k,v) return(func(x,v)) end`, and so on
	for k,v in pairs(table) do 
		base = func(base,k,v) end 
	return base 
end

function tableString(T)
	-- turns a table into a string, including all tables contained within.
	if isNotTable(T) then 
		return tostring(T) 
	end; s='{ '
	for k,v in pairs(T) do
		s=s..k..'='..ifElse(isTable(v),tableString(v),tostring(v))..', ' end
	return(string.sub(s,1,-3)..' }') end

function tfilter(Prop) -- filter table by proposition
  Yes = {}; No = {}; n = 1;
  for k, v in pairs(T) do
    if Prop(v) then Yes[k] = v else No[k] = v
  end end
  return({Yes, No})
end

function tstring(T) -- unfun implementation of tableString
  if not(type(T)==type(table))
    then return(tostring(T)) end
  s='{'
  for k, v in ipairs(T) do
    if type(k)==type(table)
      then s=s..table.print(k)
      else s=s..tostring(k) end
    s=s..'='
    if type(v)==type(table)
      then s=s..table.print(v)
      else s=s..tostring(v) end
    s=s..', ' end
  return(string.sub(s,1,-3)..'}') end

function tprint(T) 
	print(table.string(T)) end

function copyOver(T, gen)
	-- imposes the key values of the table gen onto the table T, including all internal tables,
	-- without overwriting any of T's own data.
	-- idea is that gen contains T's generic values; copyOver makes this work recursively
	for gkey, gval in pairs(gen) do
		if isTable(gval) then
			if hasNoKey(T, gkey) then T[gkey] = {} end
			if isTable(T[gkey]) then copyOver(T[gkey], gval) end
		elseif hasNoKey(T, gkey) then T[gkey] = gval
end	end end

-- fun starts here
function equipGenerics(TT)
	--[[takes a table of tables with a specified generic element (or prototype) and gives all
	 	the other tables generic values for whatever keys they haven't bothered to specify,
	 	including all internal tables.
		genFam is the key for generic family properties and methods. ]]--
	-- first, we should make sure that TT has generics; due to the recursivity of this method,
	-- it's useful to run even if they are empty, since subfamilies may have nonempty generics
	guaranteeKeysExist(TT, {'_', '__'}, {})
	for name, T in pairs(TT) do if isTable(T) then
		t = schematicType(name)
		if t.object == true then
			T.parent = TT.name; T.name = name
			if t.family == true then -- (95 is '_')
				-- then T is a family node within supergroup TT, and:
				--	0. we should make sure that it at least has concrete and
				--	   family generics, even if they are empty,
				guaranteeKeysExist(T, {'_', '__'}, {})
				-- 	1. its concrete generic should be enriched with TT's,
				copyOver(T['_'], TT['_'])
				--	2. its family generic should be enriched with TT's,
				copyOver(T['__'], TT['__'])
				--	3. it should itself be equipped with TT's generic family specifications,
				copyOver(T, TT['__'])
				--	4. and T should start equipping its generics to its members
				equipGenerics(T)
			else
				-- then T is a concrete node within TT, and should get concrete generics
				copyOver(T, TT['_']) end
			-- remove non/family requirement, just leave object requirement
			-- Extraneous work on T: first, guarantee existence of nice keys
			guaranteeKeysExist(T, {'previous'}, {})
			-- Set tag name up as a lua variable, if it exists
			if hasKey(T, 'tag') then _G[T.tag] = T end
			-- Run T's create method, if it exists
			if hasKey(T, 'create') then T:create() end
end	end	end end
--[[
function deepCreate(T)
	-- generic values should already be copied over by the time this is run
	-- T should be a family node; this method recursively applies to its subfamilies
	for k, v in pairs(T) do
		if isTable(v) then
			x = schematicType(k)
			if x.object == true then
				if hasKey(v, "tag") then _G[v.tag] = v end
					-- (e.g., for family nodes without create methods)
				if hasKey(v, 'create') then T[k]:create(k) end
				if x.family == true then deepCreate(v)
					-- since generics already copied, their existence in v indicates it's a family node
end end end end end
]]--

function makeTween(property, tag, dest, time, label, acc)
	tweenMade = false
	if label == nil or isNotString(label) then
		label = property..Y(function(f) return(function(n) if n==0 then return '' end -- :smug:
		return string.char(math.random(97,122))..f(f)(n-1) end) end)(4) end
	acc = acc or "easeOut"
	for k, v in pairs({alpha=doTweenAlpha,color=doTweenColor,x=doTweenX,y=doTweenY,angle=doTweenAngle}) do
		if property==k then v(label, tag, dest, time, acc); return label
	end end
	if property=="scale.x" or property=="scale"
		then doTweenX(label.."X",tag..".scale",dest,time,acc); tweenMade = true end
	if property=="scale.y" or property=="scale"
		then doTweenY(label.."Y",tag..".scale",dest,time,acc); tweenMade = true end
	if tweenMade
		then return label -- allows for playing with hooks later
		else return false
end end

objects = {_ = {x = 0, y = 0, alpha = 1, children = {}, updateMethods = {}},
	acc = {x = 735, y = 151, alpha = 1}, msTimer = {}, none = {}}

--[[ EXPLANATION: How to understand schematic trees
	Elements of a schematic tree are either lower or upper based on whether they provide the tree with
		determination directly and in themselves or whether they merely coordinate the determination -- in
		other words, lower elements provide the being of the thing(s) the tree schematizes, while upper elements
		coordinate functions of and among these things. It seems good to have the
		lowers form the main branching structure of the schematic tree, placing uppers perpendicular
		to this structure. So, the lowers will be the elements that are actually nodes of this tree.
	* Nodes are either abstract nodes or concrete nodes; the latter directly represent an object which is
		displayed on screen, such as a changing piece of text, whereas the former are immaterial models of objects.
	* Abstract nodes are either family nodes, which aggregate concretes cladistically, i.e. hierarchically by
		shared traits, or they are generics, which actually detail the traits common to a family.
	* Generics are either concrete generics, in which case they provide traits to all concretes of a given
		family (including those of subfamilies, unless a concrete generic of a subfamily redefines a trait, in
		which case it takes priority), or they are family generics, in which case they provide traits directly to
		all subfamilies of a given family (including subsubfamilies, unless a family generic of a subfamily...)
	* The concrete generic of any family will always be its _ key, and the family generic will always be its __
		key. Families will always be prefixed with the _ key.
--]]

function tableUpdater(this, table, opts)
	-- `this` will always be whatever thing is calling the update function, i.e., it
	-- will always be called as miku:update(table), so that `this` = `miku`;
	-- the table will contain a list of key-value pairs for the caller to implement
	for k,v in pairs(table) do
		this.previous[k] = this[k] --[[ tl;dr: works for numbers/strings, not tables
			non-tl;dr: this stores the value as it is before the update,
			in case we want to go back to it (motivating example being the temporary
			updates made for activations); but this doesn't work for tables, since
			the name of a table is really only the name of the pointer to that table
			(i.e., assignment of simple data types is by value, but assignment
			of tables is by pointer, such that the new var points to the same table).
			Need to fix this in order to make this work for tables, to allow for more
			intricate updates in the future -- probably just make a
			`function guaranteedValueTransfer(newRef, oldRef) <stuff> end`, where
			<stuff> just reduces to `newRef = oldRef` for simple data types but does
			deep copy for more complex types. ]]--
		if v=="this" then v = this[k] end
		this[k]  = v
		for i, j in pairs(this.updateMethods) do
			if i==k then j(this, v)
		end end end
	return(this)
end

updateMethods = { -- gather the disparate API methods in a single format
	x = function(this, arg) setProperty(this.tag..'.x', arg+this.dx+this.ddx) end,
	y = function(this, arg) setProperty(this.tag..'.y', arg+this.dy+this.ddy) end,
	dx = function(this, arg) setProperty(this.tag..'.x', this.x+arg+this.ddx) end,
	dy = function(this, arg) setProperty(this.tag..'.y', this.y+arg+this.ddy) end,
	ddx = function(this, arg) setProperty(this.tag..'.x', this.x+this.dx+arg) end,
	ddy = function(this, arg) setProperty(this.tag..'.y', this.y+this.dy+arg) end,
	alpha = function(this, arg) setProperty(this.tag..'.'..'alpha', arg) end,
	bdrWidth = function(this, arg) setProperty(this.tag, arg, this.bgColor) end,
	bgColor = function(this, arg) setProperty(this.tag, this.bdrWidth, arg) end,
	text = function(this, arg) setTextString(this.tag, arg) end,
	font = function(this, arg) setTextFont(this.tag, arg) end,
	color = function(this, arg) setTextColor(this.tag, arg) end,
	scalex = function(this, arg) setProperty(this.tag..'.'..'scale.x', arg) end,
	scaley = function(this, arg) setProperty(this.tag..'.'..'scale.y', arg) end,
	scale =
		function(this, arg)
			setProperty(this.tag..'.'..'scale.x', arg)
			setProperty(this.tag..'.'..'scale.y', arg)
		end,
	visibility =
		function(this, arg)
			if arg == "switch" then arg = not this.visible end
			setProperty(this.tag..'.visible', arg)
		end,
	visible =
		function(this, arg)
			if arg=="switch" then arg=not this.visible end
			setProperty(this.tag..'.visible', arg)
		end,
	active = -- activates or deactivates an element; arg2 optional argument to pass when activating element
		function(this, arg, arg2)
			if (this.active == arg) then
				return(0)
			else
				this.active = arg
				if arg then
					this:on(arg2)
				else
					this:off(arg2)
		end end end
	}

__textTags= {
	name = '__textTags', parent = '', update=tableUpdater, updateMethods=updateMethods,
	_ 	= {
		x=0, y=0, dx=0, dy=0, ddx=0, ddy=0,
		text="", width=450,
		size=35, scale=1, alpha=1, baseAlpha=1,
		alignment="center", color=colors.white, bgColor=colors.white,
		bdrWidth=0, active=true, created=false,visible=true,
		update=tableUpdater, updateMethods=updateMethods,
		create=(
			function(this)
				if not this.created then this.created = true end --maybe it's something weird, who knows
				if not this.active then return -1 end
				this.alpha = this.alpha or this.baseAlpha
				makeLuaText(this.tag, this.text, this.width, this.x+this.dx+this.ddx, this.y+this.dy+this.ddy)
				setTextSize(this.tag, this.size)
				setTextAlignment(this.tag, this.alignment)
				setTextColor(this.tag, this.color)
				setProperty(this.tag..".alpha",this.alpha)
				setProperty(this.tag..".scale.x", this.scale)
				setProperty(this.tag..".scale.y", this.scale)
				addLuaText(this.tag)
				setTextBorder(this.tag, this.bdrWidth, this.bgColor)
				if (this.font ~= "none") and (this.font ~= nil)
					then setTextFont(this.tag, this.font) end
				setProperty(this.tag .. ".visible", this.visible and true)
				return(this)
			end),
		tween=(
			function(this, table, label)
				-- like so: obj:tween({color=colors[green], 0.7 <seconds for that tween>, alpha=0.2, 2.0 <seconds>})
				n, tweenMade = 1, false
				for k, v in pairs(table) do
					if type(k) ~= type(1)
						then if label
							then label2 = label..k
							else label2 = label end
						--debugPrint(k..' '..this.tag..' '..v[1]..' '..v[2]..' '..label2)
						l = makeTween(k, this.tag, v[1], v[2], label2, v[3])
						if l == false then --[[the tween was not made]] this[k] = v[1] end --[[fuck it]] n=n+1
				end end
			return(this) end),
		activateTable=(function(this) return(this) end),
		tweenTable=(function(this) return(this) end),
		activate=(
			function(this, label, n, lazy)
				lazy = lazy or false
				n = n or 1
				if this.active and lazy then
					return(this)
				else
					this:update({active=true})
					this:update(this:activateTable()[n]):tween(this:tweenTable()[n], label)
					this:update({active=false})
				end
			return(this) end),
		on=(function(this, args) this:update({alpha=this.baseAlpha}); return(this) end),
		off=(function(this, args) this:update({alpha=0.0}); return(this) end)
	},
	__	= {
		update=(
			function(this, passTable, directTable, passDirectTable)
				--passTable is passed to concretes
				--directTable is directly taken up by family `this`
				--passDirectTable is taken up by all subfamilies of `this` (incl. subsubfamilies &c.)
				directTable = directTable or {}; passDirectTable = passDirectTable or {}
				for k, v in pairs(directTable) do this[k] = v end
				for k, v in pairs(this) do x = schematicType(k)
					if x.object == true then if x.family == true
						then v:update(passTable, passDirectTable, passDirectTable)
						else v:update(passTable) 
				end end end
				return(this)
			end),
		activate=(
			function(this, label, n)
				n = n or 1
				for k, v in pairs(this) do
					if schematicType(k).object == true
						then v:activate(label .. k, n)
				end end 
				return(this) 
			end),
		apply=(
			function(this, func) --func is f(this), applied to each object x as x:f()
				for k, v in pairs(this) do x = schematicType(k)
					if x.object == true then 
						if x.family == true then v:apply(func)
						else f(v)
				end end end 
				return(this) 
			end)
	},
	__debugFamily	= {
		tag = 'debugText',
		_ = {
			alpha=0.1, color=colors.black, x=1000, y=0,
			size=20, visible=cornerTextEnabled,
			activateTable=function(this) return {
				{alpha=1, scale=1.2}, 
				{alpha=0, scale=0.8}
				} end,
			tweenTable=function(this) return {
				{alpha={0.7, TU/2, 'easeOut'}},
				{alpha={0.1, TU, 'easeOut'}, scale={1.0, TU, 'easeOut'}}
				} end
			},
		__ = {},
		__controls1	= {
			_ = {
				dy=0
				},
			_filler	= {
					tag="fillerLight", text="|", alpha=1, dx=-105, 
					color=colors.magenta, font="wingdings-1.otf",
					activateTable=function(this) return {
						{scale=1.4}, 
						{scale=1.1}
					} end,
					tweenTable=function(this) return {
						{scale={1.0,TU/2,'easeOut'}},{scale={1.0,TU/4,'easeOut'}}
					} end
				},
			_left	= {tag="leftLight", text=arrowKeys.left, dx=-90},
			_down	= {tag="downLight", text=arrowKeys.down, dx=-75},
			_up		= {tag="upLight", text=arrowKeys.up, dx=-60},
			_right	= {tag="rightLight", text=arrowKeys.right, dx=-45},
			__modifiers	= {
				_		= {font="wingdings-3.otf"}, -- has the customary ctrl, shift, and option keys for some reason
				_space	= {tag="spaceLight", text="V", dx=-15, dy=-5},
				_shift	= {tag="shiftLight", text="X", dx=0},
				_ctrl	= {tag="ctrlLight", text="T", dx=15},
				_alt	= {tag="altLight", text="U", dx=30},
				}
			},
		__controls2	= {
			_		= {dy=20},
			_1		= {tag="oneLight", text="1", dx=-90},
			_2		= {tag="twoLight", text="2", dx=-75},
			_3		= {tag="threeLight", text="3", dx=-60},
			_4		= {tag="fourLight", text="4", dx=-45},
			_5		= {tag="fiveLight", text="5", dx=-30},
			_6		= {tag="sixLight", text="6", dx=-15},
			_7		= {tag="sevenLight", text="7", dx=0},
			_8		= {tag="eightLight", text="8", dx=15},
			_9		= {tag="nineLight", text="9", dx=30},
			_0		= {tag="zeroLight", text="0", dx=45},
			},
		__controls3 = {
			_ 		= {dy=40},
			_Q		= {tag="qLight", text="Q", dx=-90},
			_W		= {tag="wLight", text="W", dx=-75},
			_E		= {tag="eLight", text="E", dx=-60},
			_R		= {tag="rLight", text="R", dx=-45},
			_T		= {tag="tLight", text="T", dx=-30},
			_Y		= {tag="yLight", text="Y", dx=-15},
			_U		= {tag="uLight", text="U", dx=0},
			_I		= {tag="iLight", text="I", dx=15},
			_O		= {tag="oLight", text="O", dx=30},
			_P		= {tag="pLight", text="P", dx=45}
			},
		__controls4 = {
			_		= {dy=60, font="wingdings-3.otf"},
			_LArrow	= {tag="leftArrowLight", text="f", dx=0},
			_DArrow	= {tag="downArrowLight", text="i", dx=15},
			_UArrow	= {tag="upArrowLight", text="h", dx=30},
			_RArrow	= {tag="rightArrowLight", text="g", dx=45}
			},
	},
	__accFamily={
		tag = 'acc',
		_ = {
			alpha=1.0, font='none', x = 730, y = 125
			},
		__ = {},
		__horizontalBar = {
			_ = {visible = horizontalMeterEnabled},
			__EFamily = {
				tag = '__EFamily',
				_ = {
						text="E", dy=-1, size=25, color=colors.orange,
						bgColor=colors.black,bdrWidth=1.0,
						activateTable=function(this) return {
							{alpha=this.baseAlpha, scale=1.08}
							} end,
						tweenTable=function(this) return {
							{alpha={this.baseAlpha*0.7,TU}, scale={1,TU}}
							} end
					},
				_markerA = {
						tag="accMarkerA", text="8", font="wingdings-2.otf", baseAlpha=0.5, 
						size=35, dx=0, dy=12, color=colors.magenta, visible=averageMarkerEnabled
					},
				__Es = {
					tag = "__Es",
					_ = {visible=false},
					_markerEl = {tag="accMarkerEl", dx=40, baseAlpha=0, tweenTable=function(this) return{{}} end},
					_markerEr = {tag="accMarkerEr", dx=-40, baseAlpha=0, tweenTable=function(this) return {{}} end}
					}
				},
			__pointFamily = {
				tag = '__pointFamily',
				_ = {
					text='t', alpha=1, dx=0, dy=30, size=50, color=colors.green,
					visible=horizontalPositionEnabled,
					font='wingdings-1.otf', bgColor=colors.black, bdrWidth=1,
					activateTable=function(this) return {
						{scale=1.1}
						} end,
					tweenTable=function(this) return {
						{scale={1,TU, 'easeOut'}}
						} end
					},
				__ = {},
				--pointBack	= {tag="accPointBack", alpha=0.25, color=colors.white},
				_point = {tag = "accPoint"}
			},
			__barFamily=
			{
				tag = 'accBar',
				_ = {
					text= '_____________________', color=colors.white, bgColor = colors.white,
					alpha = 0.3, visible=horizontalBarEnabled,
					activateTable=function(this) return {
						{alpha=0.6, scale=1.03}
						} end,
					tweenTable=function(this) return {
						{alpha={0.3,TU,'easeOut'}, scale={1,TU,'easeOut'}}
						} end
					},
				__ = {},
				_barUpper = {tag="accBar1", bdrWidth=0.5, color=colors.white, bgColor=colors.black},
				_barLower = {tag="accBar2", dx=-1, bgColor=colors.white, bdrWidth=1.0}
			},
			__greenFamily = {
				tag = '__greenFamily',
				_  = {
					color=colors.green, alpha=0.4, bdrWidth=0.5, bgColor=colors.green,
					visible=perfectRangeEnabled, y = 155,
					activateTable=function(this) return {
						{alpha=1, color=colors.lightGreen, scale=1.25}
						} end,
					tweenTable=function(this) return {
						{alpha={0.4,TU,'quadOut'}, color={colors.green,TU,'quadOut'}, scale={1,TU/2,'quadOut'}}
						} end
					},
				_max = {tag='accMax', text='       |     |       ', dy=12},
				_center = {tag='accCenter', text='|', dy=7, size=50},
				_fillIn = {
					tag='accFillIn', text = '      _______      ', dy=0, bgColor=colors.green,size=32,
					activateTable=function(this) return {
						{alpha=1, color=colors.lightGreen, scale=1.03}
						} end
					}
			},
			__warnFamily =
			{
				tag = "warnBar",
				_ = {
					size=50, dy=4, color=colors.white, font='webdings.otf',
					alpha = 0.5,bgColor=colors.black,visible=warningLightsEnabled,
					activateTable=function(this) return {
						{color=this.activeColor, scale=2,alpha=1},
						{alpha=0.5,scale=1.05,dx=this.dx*1.03}
						} end,
					tweenTable=function(this) return {
						{color={colors.white, 2*TU},alpha={0.5,TU/2},scale={1.0,TU}},
						{scale={1.0,TU, "easeOut"}, x={this.x+this.dx/1.03,TU}, dx={this.dx/1.03}}
						} end
					},
				_warnLeftA	= {tag='accWarnLeftA', dx=219+8, text='4', activeColor=colors.orange},
				_warnLeftB	= {tag='accWarnLeftB', dx=219+24, text='4', activeColor=colors.red},
				_warnRightA	= {tag='accWarnRightA', dx=-223-2, text='3', activeColor=colors.orange},
				_warnRightB	= {tag='accWarnRightB', dx=-223-18, text='3', activeColor=colors.red}
			}, -- __warnFamily
		},
		__vertFamily=
		{
			tag			= '__vertFamily',
			_			= {
				y=70, alpha=0.4, dy=0, size=50, color=colors.white, dx=0, bgColor=colors.black,
				bdrWidth=1,visible=verticalMeterEnabled,
				activateTable=function(this) return {
					{scale=1.1}
					} end,
				tweenTable=function(this) return {
					{scale={1,TU, 'easeOut'}}
					} end},
			_target = {
				tag='vertTarget', ddy=50, size=5, 
				text='----------------------------------', bdrWidth=1.0,
				bgColor=colors.white
				},
			_bar = {tag='vertBar', ddy=28, size=50, text='+'}
		},
		__pDiamondFamily = {
			tag = "pDiamondFigure",
			_ = {
				baseAlpha = 0.0, alpha=0.0, font='wingdings-1.otf', text='w', bdrWidth=1.5,
				bgColor=colors.black, diamondTweenMult=1.0, visible=pMainDiamondEnabled,num=1,
				activateTable=function(this) return {
					{alpha=1.0, scale=1.2},
					{alpha=0.5*this.num, scale=1.05},
					{alpha=0.25
						--,scale=1.05, dx=this.dx0+this.dxmove*0.05, dy=this.dy0+this.dymove*0.05
					},
					{scale=1.20, dx=this.dx0+this.dxmove*0.20, dy=this.dy0+this.dymove*0.20},
					{scale=1.05, dx=this.dx0+this.dxmove*0.05, dy=this.dy0+this.dymove*0.05},
					{alpha=1.0},
					} end,
				tweenTable=function(this) return {
					{alpha={this.baseAlpha, TU/2, 'easeOut'}, scale={1.0,   TU/2, 'easeOut'}},
					{alpha={this.baseAlpha, TU/2, 'easeOut'}, scale={1.0,   TU/2,'easeOut'}},
					{alpha={this.baseAlpha, TU/2*this.diamondTweenMult, 'quadOut'}
						--,scale={1.0,  TU/3, 'easeOut'}, x={this.x+this.dx0, TU/3, 'easeOut'}
						--,y={this.y+this.dy0, TU/3, 'easeOut'}, dx={this.dx0}, dy={this.dy0}
					},
					{scale={1.0, TU/2, 'easeOut'}, x={this.x+this.dx0, TU/2, 'easeOut'},
						y={this.y+this.dy0, TU/2, 'easeOut'}, dx={this.dx0}, dy={this.dy0}},
					{scale={1.0,  TU/3, 'easeOut'}, x={this.x+this.dx0, TU/3, 'easeOut'},
						y={this.y+this.dy0, TU/3, 'easeOut'}, dx={this.dx0}, dy={this.dy0}},
					{alpha={this.baseAlpha, TU/4*this.diamondTweenMult, 'elasticIn'}}
					} end
				},
			_pDiamondBG = {
				tag = "pDiamondBG", text = "v", color = colors.black, size = 400, dx = 0, dx0 = 0,
				dy = -185, dy0 = 0, dxmove = 0, dymove = 0, alpha = 0.15, baseAlpha = 0.15, visible = pDiamondBGEnabled
				},
			__pDiamondButtonFamily = {
				_ = {size = 289, visible = pDiamondButtonsEnabled},
				_pDiamondUpButton = {
					tag = "pDiamondUpButton", color = colors.green, dx = 0, dx0 = 0,
					dy = -212, dy0 = -212, dxmove=0, dymove=0 -- -87
					},
				_pDiamondDownButton = {
					tag = "pDiamondDownButton", color = colors.cyan, dx = 0, dx0 = 0,
					dy = -40, dy0 = -40, dxmove=0, dymove=0 --87
					},
				_pDiamondLeftButton = {
					tag = "pDiamondLeftButton", color = colors.magenta, dx = -86, dx0 = -86,
					dy = -126, dy0 = -126, dxmove=0-- -87
					, dymove=0
					},
				_pDiamondRightButton = {
					tag = "pDiamondRightButton", color = colors.red, dx = 86, dx0 = 86,
					dy = -126, dy0 = -126, dxmove=0 --87
					, dymove=0
					},
				},
			__pDiamondArrowFamily = { -- player-activated diamond arrows
				_ = {size=100, font="webdings.otf", bgColor = colors.black},
				_pDiamondUpArrow = {
					tag = "pDiamondUpArrow", text="5", color = "5fe8df", dx = 4, dx0 = 4,
					dy = -148, dy0 = -148, dxmove=0, dymove=-87, 
					},
				_pDiamondDownArrow = {
					tag = "pDiamondDownArrow", text="6", color ="5ce5e9", dx = 4, dx0 = 4,
					dy = 103, dy0 = 103, dxmove=0, dymove=87, 
					},
				_pDiamondLeftArrow = {
					tag = "pDiamondLeftArrow", text="3", color = "57eae3", dx = -122, dx0 = -122,
					dy = -19, dy0 = -19, dxmove=-87, dymove=0, 
					},
				_pDiamondRightArrow = {
					tag = "pDiamondRightArrow", text="4", color = "5ce8e3", dx = 125, dx0 = 125,
					dy = -19, dy0 = -19, dxmove=87, dymove=0, 
					},
			}, 
			__eDiamondProxyFamily = { -- enemy-activated inner diamond arrows, on player side
				_ = {size=100, font="webdings.otf", color = colors.purple, bgColor = colors.black, alpha=0.0, 
					baseAlpha=0.0, diamondTweenMult=2},
				_eDiamondUpProxy = {
					tag = "eDiamondUpProxy", text="5", dx = 4, dx0 = 4, color = "ac00e6",
					dy = -148+40, dy0 = -148+40, dxmove=0, dymove=-87, 
					},
				_eDiamondDownProxy = {
					tag = "eDiamondDownProxy", text="6", dx = 4, dx0 = 4, color = "b100e2",
					dy = 103-40, dy0 = 103-40, dxmove=0, dymove=87, 
					},
				_eDiamondLeftProxy = {
					tag = "eDiamondLeftProxy", text="3", dx = -121+40, dx0 = -121+40, color = "ac00ea",
					dy = -19, dy0 = -19, dxmove=-87, dymove=0, 
					},
				_eDiamondRightProxy = {
					tag = "eDiamondRightProxy", text="4", dx = 126-40, dx0 = 126-40, color = "af06e9",
					dy = -19, dy0 = -19, dxmove=87, dymove=0, 
					},
			}, 
			_pDiamondRing = {
				tag="pDiamondRing", color = colors.white, size=289, dy=-108, dy0=-108, dymove=0,
				dx=1, dx0=1, dxmove=0, text='8', font='wingdings-2.otf', bgColor = colors.white,
				visible=pDiamondRingEnabled
				},
			_pDiamondRing2 = {
				tag="pDiamondRing2", color = colors.brightGold, size=315, dy=-139, dy0=-139, dymove=0,
				dx=1, dx0=1, dxmove=0, text='l', font='wingdings-1.otf', bgColor = colors.gold, baseAlpha=0.05,
				alpha=0.05, visible=pDiamondRing2Enabled, num=2
				}
		},
		__histogramFamily = {
			tag = "histogram", bars = histogramBars,
			_ = {
				text = "_", alpha=0.3, baseAlpha = 0.3, color=colors.magenta, size=750/histogramBars, bgColor = colors.black, 
				activateTable = function(this, k) k = k or 1; return {{alpha=0.3+(2-0.3)*k, scale=1.0+0.4*k}} end, 
				tweenTable = function(this) return {{alpha={this.baseAlpha, TU*2, 'easeOut'}, scale={1.0,   TU, 'easeOut'}}} end
			}
		}
	}, -- __accFamily
	__enemySide = {
		tag = 'eacc',
		_ = {alpha=0.5, font='none', x = 90, y = 125},
		__ = {},
		__eDiamondFamily = {
			tag = "eDiamondFigure",
			_ = {
				baseAlpha = 0.0, alpha=0.0, font='wingdings-1.otf', text='w', bdrWidth=1.5,
				bgColor=colors.black, diamondTweenMult=1.0,visible=eMainDiamondEnabled,num=1,
				activateTable=function(this) return {
					{alpha=1.0, scale=1.2},
					{alpha=0.5, scale=1.05},
					{alpha=1
						--,scale=1.05, dx=this.dx0+this.dxmove*0.05, dy=this.dy0+this.dymove*0.05
					},
					{scale=1.20, dx=this.dx0+this.dxmove*0.20, dy=this.dy0+this.dymove*0.20},
					{scale=1.05, dx=this.dx0+this.dxmove*0.05, dy=this.dy0+this.dymove*0.05},
					{alpha=1.0}
					} end,
				tweenTable=function(this) return {
					{alpha={this.baseAlpha, TU/2, 'easeOut'}, scale={1.0,   TU/2, 'easeOut'}},
					{alpha={this.baseAlpha, TU/2, 'easeOut'}, scale={1.0,   TU/2,'easeOut'}},
					{alpha={this.baseAlpha, TU/2*this.diamondTweenMult, 'quadOut'}
						--,scale={1.0,  TU/3, 'easeOut'}, x={this.x+this.dx0, TU/3, 'easeOut'}
						--,y={this.y+this.dy0, TU/3, 'easeOut'}, dx={this.dx0}, dy={this.dy0}
					},
					{scale={1.0, TU/2, 'easeOut'}, x={this.x+this.dx0, TU/2, 'easeOut'},
						y={this.y+this.dy0, TU/2, 'easeOut'}, dx={this.dx0}, dy={this.dy0}},
					{scale={1.0,  TU/3, 'easeOut'}, x={this.x+this.dx0, TU/3, 'easeOut'},
						y={this.y+this.dy0, TU/3, 'easeOut'}, dx={this.dx0}, dy={this.dy0}},
					{alpha={this.baseAlpha, TU/4*this.diamondTweenMult, 'elasticIn'}}
					} end
				},
			_eDiamondBG = {
				tag="eDiamondBG", text="v", color=colors.black, size=400, dx=0, dx0=0, dy=-185, dy0=0,
				dxmove=0, dymove=0, alpha=0.0, baseAlpha=0.0, visible=eDiamondBGEnabled
				},
			__eDiamondButtonFamily = {
				_ = {size = 289, visible = eDiamondButtonsEnabled},
				_eDiamondUpButton = {
					tag = "eDiamondUpButton", color = colors.green, dx = 0, dx0 = 0, dy = -212,
					dy0 = -212, dxmove=0, dymove=-87
					},
				_eDiamondDownButton = {
					tag = "eDiamondDownButton", color = colors.cyan, dx = 0, dx0 = 0, dy = -40,
					dy0 = -40, dxmove=0, dymove=8
					},
				_eDiamondLeftButton = {
					tag = "eDiamondLeftButton", color = colors.magenta, dx = -86, dx0 = -86, dy=-126,
					dy0 = -126, dxmove=-87, dymove=0
					},
				_eDiamondRightButton = {
					tag = "eDiamondRightButton", color = colors.red, dx = 86, dx0 = 86, dy = -126,
					dy0 = -126, dxmove=87, dymove=0
					},
				},
			__eDiamondArrowFamily = { -- enemy diamond arrows
				_ = {size=100, font="webdings.otf", bgColor = colors.black, diamondTweenMult=2},
				_eDiamondUpArrow = {
					tag = "eDiamondUpArrow", text="5", color = "b100e2", dx = 4, dx0 = 4,
					dy = -148, dy0 = -148, dxmove=0, dymove=-87, 
					},
				_eDiamondDownArrow = {
					tag = "eDiamondDownArrow", text="6", color ="ac00e6", dx = 4, dx0 = 4,
					dy = 103, dy0 = 103, dxmove=0, dymove=87, 
					},
				_eDiamondLeftArrow = {
					tag = "eDiamondLeftArrow", text="3", color = "af06e9", dx = -122, dx0 = -122,
					dy = -19, dy0 = -19, dxmove=-87, dymove=0, 
					},
				_eDiamondRightArrow = {
					tag = "eDiamondRightArrow", text="4", color = "ac00ea", dx = 125, dx0 = 125,
					dy = -19, dy0 = -19, dxmove=87, dymove=0, 
					},
			}, 
			__pDiamondProxyFamily = { -- player diamond inner arrows, on enemy side
				_ = {size=100, font="webdings.otf", color = colors.cyan, alpha=0.0, baseAlpha=0.0},
				_pDiamondUpProxy = {
					tag = "pDiamondUpProxy", text="5", dx = 3, dx0 = 3, color = "57eae3",
					dy = -128+32, dy0 = -128+32, dxmove=0, dymove=-87, 
					},
				_pDiamondDownProxy = {
					tag = "pDiamondDownProxy", text="6", dx = 3, dx0 = 3, color = "5ce8e3", 
					dy = 83-32, dy0 = 83-32, dxmove=0, dymove=87, 
					},
				_pDiamondLeftProxy = {
					tag = "pDiamondLeftProxy", text="3", dx = -102+32, dx0 = -102+32, color = "5ce5e9",
					dy = -19, dy0 = -19, dxmove=-87, dymove=0, 
					},
				_pDiamondRightProxy = {
					tag = "pDiamondRightProxy", text="4", dx = 105-32, dx0 = 105-32, color = "5fe8df",
					dy = -19, dy0 = -19, dxmove=87, dymove=0, 
					},
			}, 
			_eDiamondRing = {
				tag="eDiamondRing", color = colors.black, size=0--289
				, dy=-108, dy0=-108, dymove=0,  dx=1, dx0=1, dxmove=0, text='8', font='wingdings-2.otf',
				bgColor = colors.black,baseAlpha=0, alpha=0
				},
			}
		},
	__preview = {
		_ = {x=800, y=800}
		},
	__textFamily = {
		_ = {text='', position='absolute', color=colors.white, bgColor = colors.black, size=24},
		__accTextFamily = {
			tag='accText',
			_ = {x=735, y=151},
			_msText ={
				tag='msText', text='0.0', font='vultures.otf', width=500, dx=-30, dy=30, size=60,
				alpha=0.3, visible=msDisplayEnabled
				},
			},
		_ratingsString = {tag='ratings', width=600, x=330, y=20, size=18},
		_songTitle = {tag='songTitle', width=1000, x=205, y=40},
		_modifier = {
			tag='modifier', size=400, x=400, y=400, baseAlpha=0.5, text='F', alpha=0.5,
			active=false, font='wingdings-3.otf'
			}
		},
	}
__charTags = { --[[
		this does not have to implement the actual character sprites in any way; let the game handle it
		it's sufficient for this to serve as a unified interface for tracking and updating character variables
	]]
	tag	=	'chars', index = 1, order={'_bf', '_gf', '_dad'}, names={"bf", "gf", "dad"},
	_	= {
		create = function(this) return nil end,
		update = tableUpdater, updateMethods = updateMethods, scale = 1.0 
		},
	__characters = {
		tag='characters',
		_ = {
			updateMethods = {
				-- character updates work differently from text updates in some cases, the same in others
				-- this is why it's useful to update via tiny swappable methods with a common applicator, like swappable drill bits:  
				-- the same generic table equipment used for the UI elements can also be used for updateMethods
				-- it's not easy being this clever
				x = function(this, arg) setCharacterX(this.tag, arg) end,
				y = function(this, arg) setCharacterY(this.tag, arg) end,
				dx = function(this, arg) setCharacterX(this.tag, getCharacterX(this.tag)+arg) end,
				dy = function(this, arg) setCharacterY(this.tag, getCharacterY(this.tag)+arg) end,
				scale = 
					function(this,arg) 
						setProperty(this.fullName .. ".scale.x", arg)
						setProperty(this.fullName .. ".scale.y", arg) 
					end,
				dscale = 
					function(this,arg) 
						setProperty(this.fullName..".scale.x",
						getProperty(this.fullName..".scale.x")*arg)
						setProperty(this.fullName .. ".scale.y", getProperty(this.fullName .. ".scale.y") * arg) 
					end
			}	},
		_bf = {
			tag='bf', color=colors.cyan, accX=735, msX=800, fullName='boyfriend', cacheNum=1,
			cached={'bf', 'gf'}, num="three"
			},
		_gf = {
			tag='gf', color=colors.red, accX=408, msX=400, fullName='gf', cacheNum=1,
			cached={'gf', 'no'}, num="two"
			},
		_dad = {
			tag='dad', color=colors.purple, accX=90, msX=0, fullName='dad', cacheNum=1,
			cached={'dad', 'mom'}, num="one"
			}
		},
	char = '_bf',
	__icons = {
		names = {"BFIcon", "GFIcon", "DadIcon"},
		_ = {
			activateTable=function(this) return {
				{alpha=1, scale=1}
				} end,
			alpha=0, alphaEnd=0.0,
			tweenTable=function(this) return {
				{
					alpha={this.alphaEnd,this.speed,this.alphaRate},
					x={this.xEnd, this.speed, this.xRate,},
					y={this.yEnd, this.speed, this.yRate}
					}
				} end,
			xEnd=550, yEnd=250, speed=0.7, size=50, font="webdings.otf",
			alphaRate="quadOut", xRate="linear", yRate="quadOut", bgColor=colors.black
			},
		BFIcon	= {
			tag="BFIcon", text="U", x=700, y=500, alphaRate="easeOut", color=colors.cyan
			},
		GFIcon	= {
			tag="GFIcon", text="Y", x=500, y=350, yRate="cubeOut", color=colors.red
			},
		DadIcon	= {
			tag="DadIcon", text="M", x=300, y=400, xRate="quadOut", yRate="quadIn",
			xEnd=550, yEnd=250, color=colors.purple, font="wingdings-1.otf"
			}
		}
	}
function onCreate()
	if(opponentPlay) then
		__textTags.__accFamily._.x = 90
		__textTags.__enemySide._.x = 730
		__charTags.index = 3
	end
	if(histogramEnabled) then
		if(histogramBars % 2 == 0) then 
			histogramBars = histogramBars + 1 -- make sure it's odd, so the middle is clearly defined
		end
		if(#kernel > 1) then -- normalize the kernel array so convolution w/ histogram preserves mass
			sumK = 0 
			for i = 1, #kernel do sumK = sumK + kernel[i] end
			for i = 1, #kernel do kernel[i] = kernel[i] / sumK end
			kernelEnabled = true;
			kernelCenter = math.floor((#kernel + 1)/2); kernelWidth = math.floor((#kernel - 1)/2)
			kernelAmplitude = kernel[kernelCenter]
		else
			kernelEnabled = false; kernelAmplitude = 1
		end
		histb = histogramBars; histx = __textTags.__accFamily._.x; histy = __textTags.__accFamily._.y-25;
		-- 45 ms on each side, 5 px / ms = 225 px/side
		histwidth = 450/histb; 
		barColor = colors.orange; 
		for histn = 1, histb do
			if 2*histn-1 - histb/3 > histb then
				barColor = colors.blue
			elseif 2*histn-1 + histb/3 < histb then
				barColor = colors.orange
			else
				barColor = colors.green
			end
			--makeLuaSprite(("hist2Bar"..histn), "", histx + ((2*histn-1)-histb) * histwidth, histy)
			--makeGraphic(("hist2Bar"..histn), histwidth, 10, barColor)
			__textTags.__accFamily.__histogramFamily["_bar"..histn] = {tag = "histBar"..histn, dx = histn * histwidth - (histb + 1) * histwidth * 0.5, color = barColor, bgColor = barColor, dy = histb/2}
		end
	end
	equipGenerics(__textTags)
	equipGenerics(__charTags)
	_G['chars'] = __charTags
	index=chars.index
	char = characters[chars.order[chars.index]]
	--for idx, base in ipairs(chars.order) do
	--	for _idx, new in ipairs(characters[base].cached) do
	--		addCharacterToList(new, characters[base].tag)
	--end end
	ringBarList = {__EFamily, __pointFamily, accBar, __greenFamily, __vertFamily, warnBar, pDiamondRing, accText}
end

function onCreatePost()
	tracker = {
		songPosition = 0, lastID = 0, offset = getPropertyFromClass('ClientPrefs','ratingOffset'),
		strumTime = 0, difference = 0,
		update = function(this, id, isNote)
			this.songPosition = getPropertyFromClass('Conductor', 'songPosition')
			if not(isNote) then return this.songPosition end
			this.strumTime = getPropertyFromGroup('notes', id, 'strumTime')
			this.difference = -this.songPosition + this.strumTime + this.offset
			return this.difference end
	}

	avgCalc = {
		sum=0, absSum = 0, N=0,
		update = function(this, difference)
			this.sum=this.sum+difference; this.N=this.N+1
			this.absSum=this.absSum+math.abs(difference)
			this.avg=this.sum/this.N
			this.absAvg=this.absSum/this.N
			return this.avg end
	}
end

function onBeatHit()
	-- set custom options on first beat, since trying earlier messes up
	-- really stupid way to do it, but it works well and gives clear indication of activation
	if doneUpdate == 0 then
		pDiamondLeftButton:activate('dlBeat2', 4); pDiamondDownButton:activate('ddBeat2', 4)
		pDiamondUpButton:activate('duBeat2', 4); pDiamondRightButton:activate('drBeat2', 4)
		debugPrint(songName)
		-- whatever is not changed will remain the same
		customUI = {}
		for kF, vF in pairs({color={}, bgColor={}}) do
			customUI[kF] = vF
			for kX, vX in pairs({p={}, e={}}) do
				customUI[kF][kX] = vX
				for kT, vT in pairs({Button={}, Arrow={}, Proxy={}, Diamond="this"}) do
					customUI[kF][kX][kT] = vT
					if not(vT == "this") then
					for kD, vD in pairs({Left="this", Right="this", Up="this", Down="this"}) do
						customUI[kF][kX][kT][kD] = vD
		end	end	end	end	end
		custom_PUB_color="this"; custom_PUB_bgColor="this"; custom_PDB_color="this"; custom_PDB_bgColor="this";
		custom_PLB_color="this"; custom_PLB_bgColor="this"; custom_PRB_color="this"; custom_PRB_bgColor="this";
		custom_PUB_color="this"; custom_PDB_color="this"; custom_PLB_color="this"; custom_PRB_color="this";
		custom_EUB_color="this"; custom_EUB_bgColor="this"; custom_EDB_color="this"; custom_EDB_bgColor="this";
		custom_ELB_color="this"; custom_ELB_bgColor="this"; custom_ERB_color="this"; custom_ERB_bgColor="this";
		custom_EUB_color="this"; custom_EDB_color="this"; custom_ELB_color="this"; custom_ERB_color="this";
		custom_PDia_color = "this"; custom_EDia_color = "this"
		sN = songName:lower()
		if sN == "dystychiphobia" then doneUpdate = 1
			customUI.color.p.Button = {Up="8ba9c4",Down="8ba9c4",Left="e6dd91",Right="e6dd91"}
			customUI.color.e.Button = customUI.color.p.Button
			customUI.color.p.Diamond = colors.white; customUI.color.e.Diamond = colors.white;
			custom_PUB_color="8ba9c4"; custom_PDB_color="8ba9c4"; custom_PLB_color="e6dd91"; custom_PRB_color="e6dd91"
			custom_EUB_color="8ba9c4"; custom_EDB_color="8ba9c4"; custom_ELB_color="e6dd91"; custom_ERB_color="e6dd91"
			custom_PDia_color = colors.white; custom_EDia_color = colors.white
		elseif sN == "invincible" then doneUpdate = 1
			custom_ELB_color="A00000"; custom_ERB_color="C00060"; custom_EUB_color="FF0000"; custom_EDB_color="900040"
			custom_PDia_color = colors.white
			custom_PLB_color = "00ffff"; custom_PRB_color = "4ddbff"; custom_PUB_color = "0099cc"; custom_PDB_color = "80dfff"
			eDiamondBG:update({diamondTweenMult=6.0})
		elseif sN == "salvation" then doneUpdate = 1
			custom_EDia_color = "ffffff";
			custom_ELB_color = "fffa91"; custom_EUB_color = "ffffff"; custom_ERB_color="ffec73"; custom_EDB_color = "ff5a47"
		elseif sN == "where are you" then doneUpdate = 1
			custom_EDia_color = "57e3ff"; custom_PDia_color = "ffffff"
			custom_ELB_color = "ff7f24"; custom_EUB_color = "00e02d"; custom_ERB_color = "803e00"; custom_EDB_color = "a300cc"
			custom_PLB_color = "57e3ff"; custom_PUB_color = "ff1a53"; custom_PRB_color = "0044cc"; custom_PDB_color = "821717"
		elseif sN == "legacy" then doneUpdate = 1
			custom_EDia_color = "ffff00"; custom_PDia_color = "ffff00"; custom_ELB_color = "ff1a1a"; custom_ERB_color = "660000"
			custom_EUB_color = "ff9900"; custom_EDB_color = "ffcc99"
		elseif sN == "unknown-mother-goose" then doneUpdate = 1
			custom_PLB_color=colors.black;custom_PDB_color=colors.black;custom_PUB_color=colors.black;custom_PRB_color=colors.black;
			custom_PLB_bgColor=colors.magenta;custom_PDB_bgColor=colors.cyan;custom_PUB_bgColor=colors.green;custom_PRB_bgColor=colors.red;
		elseif sN == "intertwined-ex" then doneUpdate = 1
			custom_EUB_color=RGBtoHex(158,0,85);custom_ELB_color=RGBtoHex(190,0,160);custom_EDB_color=RGBtoHex(80,0,50);custom_ERB_color=RGBtoHex(92,78,80);
			custom_PUB_color=RGBtoHex(108,204,202);custom_PDB_color=RGBtoHex(69,138,206);custom_PRB_color=RGBtoHex(182,143,205);custom_PLB_color=RGBtoHex(79,133,191)
		elseif sN=="bring-it-on" or sN=="electric-angel" or sN=="yong" or sN=="childish-war" then doneUpdate = 1
			custom_EUB_color='f9df00';custom_ELB_color='f3f200';custom_EDB_color='fbeaf7';custom_ERB_color='ffe6ff';
			custom_PUB_color='554741';custom_PDB_color='fdec4f';custom_PRB_color='fdff5a';custom_PLB_color='515151'
		end
		--debugPrint(sN, doneUpdate, custom_PLB_color)
		if doneUpdate == 1 then -- i.e., if any of the above matched
			D = {Button={"Button", "Arrow", "Proxy"}, Arrow={"Arrow", "Proxy"}, Proxy={"Proxy"}}
			for kX, vX in pairs({p="P", e="E"}) do
				for kT, vT in pairs({Button="B", Arrow="A", Proxy="P"}) do
					for kTi, vTi in ipairs(D[kT]) do
						for kD, vD in pairs({Left="L", Right="R", Up="U", Down="D"}) do
							_G[kX..'Diamond'..kD..vTi]:update({color=_G["custom_"..vX..vD..vT.."_color"]})
			end end end end
			pDiamondBG:update({color=custom_PDia_color}); eDiamondBG:update({color=custom_EDia_color})
		end
		doneUpdate = 2
	end
	-- continuing, check if we're on the first beat (almost all songs are in 4/4 time) and make things bounce accordingly
	if curBeat%4 == 0 then
		--accBar:activate('beat'); warnBar:activate('warnBeat',2); accMarkerA:activate('ABeat');
		accPoint:activate('pointBeat');
		--if cornerTextEnabled then fillerLight:activate('FBeat2',1) end
		--pDiamondBG:activate('dBeat2', 1); pDiamondRing:activate('dRiBeat2', 1)
		pDiamondRing2:tween({alpha={0.5,0.001, 'quadOut'}}):tween({alpha={0.0, TU*2, 'easeOut'}}) -- :activate('dRi2Beat2', 1)
		--pDiamondLeftButton:activate('dlBeat2', 4); pDiamondDownButton:activate('ddBeat2', 4)
		--pDiamondUpButton:activate('duBeat2', 4); pDiamondRightButton:activate('drBeat2', 4)
		eDiamondBG:activate('eDBeat2', 1); eDiamondRing:activate('eDRiBeat2', 1)
		--eDiamondLeftButton:activate('eDlBeat2', 4); eDiamondDownButton:activate('eDdBeat2', 4)
		--eDiamondUpButton:activate('eDuBeat2', 4); eDiamondRightButton:activate('eDrBeat2', 4)
		lastBeatMain = true
	else
		--if cornerTextEnabled then fillerLight:activate('FBeat1',2) end
		--pDiamondBG:activate('dBeat1', 2); --pDiamondRing:activate('dRiBeat1', 2)
		--pDiamondRing2:tween({alpha={1.0,0.01, 'quadOut'}}):activate('dRi2Beat1', 2)
		--pDiamondLeftButton:activate('dlBeat1', 5); pDiamondDownButton:activate('ddBeat1', 5)
		--pDiamondUpButton:activate('duBeat1', 5); pDiamondRightButton:activate('drBeat1', 5)
		--eDiamondBG:activate('eDBeat1', 2); eDiamondRing:activate('eDRiBeat1', 2)
		--eDiamondLeftButton:activate('eDlBeat1', 5); eDiamondDownButton:activate('eDdBeat1', 5)
		--eDiamondUpButton:activate('eDuBeat1', 5); eDiamondRightButton:activate('eDrBeat1', 5)
		lastBeatMain = false
end	end


function onUpdate(elapsed)
	TU = 60/curBpm
	index = chars.index
	char = characters[chars.order[chars.index]];
	for k, v in pairs(keyList) do
		T = keysPressed[k]
		P = getPropertyFromClass("flixel.FlxG", "keys.pressed."..v) or keyboardPressed(v)
		P = type(P)=="boolean" and P or false
		keysPressed[k] = P; keysJustPressed[k] = P and not T; keysJustReleased[k] = T and not P
		if cornerTextEnabled then
			if keysJustPressed[k] then _G[k.."Light"]:activate(k.."Press", 1) end
			if keysJustReleased[k] then _G[k.."Light"]:activate(k.."Release", 2) end
		end
	end
	--if keysJustPressed.q then debugPrint('BIGGER') end
	--if keysJustPressed.w then debugPrint('smaller') end
	if (not keysPressed.shift) and (not keysPressed.ctrl) then
		if keysPressed.q then char:update({dscale=1+growRate}) end
		if keysPressed.w then char:update({dscale=1-growRate}) end
		if keysJustPressed["e"] and false then --flip char animation directions... way too easy to press accidentally
			--debugPrint("hehe")
			directions.leftArrow = directions.right; directions.right = directions.left;
			directions.left = directions.rightArrow; directions.rightArrow = directions.right
			debugPrint('Left and right flipped')
		end
		for k, v in pairs(directions) do
			if keysPressed[k] then
				if(pDiamondArrowsEnabled) then
					_G[pDiamondArrowTags[k]]:activate(pDiamondArrowTags[k].."Beat", 6)
				end
				if(pDiamondProxyEnabled) then
					_G[pDiamondProxyTags[k]]:activate(pDiamondProxyTags[k].."Beat", 6) --update({alpha=1}):tween({alpha={0.0,0.1,'easeOut'}}, pDiamondProxyTags[k].."Press")
				end
				characterPlayAnim(char.tag, v, true)
				if(pDiamondRingEnabled and k=="space") then pDiamondRing:activate('DRiBeatGold', 3) end
		end end
		for k,v in ipairs(Cap321) do
			if keysJustPressed[v] then
				ex=char.accX+0.01; chars.index = k; char = characters[chars.order[k]]
				for kk, vv in pairs({__vertFamily, __EFamily, __pointFamily, accBar, __greenFamily,
				warnBar, pDiamondRing, accText, histogram}) do
					vv:update({x=char.accX})
	end	end	end	end
	if keysPressed.shift and (not keysPressed.ctrl) then
		modifier:update({text='X', active=true})
		for k, v in pairs(keyMovements) do
			if keysPressed[k] then char:update({dx=v[1], dy=v[2]})
		end end
		for k, v in ipairs(Cap321) do
			if keysJustPressed[v] and not(v==char.num) then
				b = boyfriendName
				triggerEvent('Change Character', chars.names[1], ({boyfriendName, gfName, dadName})[k])
				triggerEvent('Change Character', chars.names[k], b)
	end	end end
	if keysPressed.ctrl then
		for k, v in ipairs(Cap321) do
			if keysJustPressed[v] then
				thischar = characters['_'..chars.names[k]]
				--if((v==1 and opponentPlay) or (v==3 and not opponentPlay)) 
					--then ourprefix="e";
					--else ourprefix="p" end
				--for i1, v1 in ipairs({"Left", "Down", "Up", "Right"}) do
					--for i2, v2 in ipairs({"Arrow", "Proxy"}) do
						--debugPrint(ourprefix.."Diamond"..v1..v2)
						--_G[ourprefix.."Diamond"..v1..v2]:update({color=thischar.color})
				--end end
				if thischar.cacheNum == #thischar.cached then thischar.cacheNum = 1 else thischar.cacheNum = thischar.cacheNum + 1 end
					triggerEvent('Change Character', chars.names[k], thischar.cached[thischar.cacheNum])
		end end
		if keysPressed.shift then
			modifier:update({text='TX', active=true})
		else
			modifier:update({text='T', active=true})
		end
		if keysJustPressed.q then
			debugPrint("f")
			debugText:update({visible="switch"})
end end end

function goodNoteHit(id, noteData, noteType, isSustain)
	for k, v in pairs(noteDirections) do -- noteDirections = {{"Left", "l"}, {"Down", "d"}, {"Up", "u"}, {"Right", "r"}}
		if noteData+1 == k then _G[playerDiamondPrefix.."Diamond"..v[1].."Button"]:activate(playerDiamondPrefix..v[2].."ButtonBeat", 3, true)
	end end
	tracker:update(id, true); d = tracker.difference; avgCalc:update(d)
	if not(isSustain) then
		if d*d < 225 then accMax:activate("accLabel")
			accCenter:activate("accLabel2"); accFillIn:activate("accLabel3") end
		--accMarkerEl:update({dx=-5*math.min(avgCalc.absAvg, 45)})
		--accMarkerEr:update({dx=-accMarkerEl.dx})
		if(averageMarkerEnabled) then
			accMarkerA:update({dx=-5*math.min(avgCalc.avg, 45)})
		end
		if(d < -15)
			then c='blue';
			if(warningLightsEnabled and (d < -45))
				then accWarnLeftA:activate();
				if(d < -75)
					then accWarnLeftB:activate()
						
			end end
		elseif(d < 15)
			then c='green'
		elseif(d >= 15)
			then c='orange';
			if(warningLightsEnabled and (d > 45))
				then accWarnRightA:activate();
				if(d > 75)
					then accWarnRightB:activate()
			end end
		else c = "black" end
		if(verticalMeterEnabled) then
			vertBar:update({dy = 0.02*curBpm*d, color=colors[c]})
		end
		accPoint:update({dx = -5 * math.abs(d) / d * math.min(math.abs(d), 45) - 2, color=colors[c]}):tween(
			{alpha={0.1,3}, color={colors.white, 2}}, "accTween")
		--accPoint:activate("flashLabel")
		if(msDisplayEnabled) then
			msText:update({text = string.format("%.1f", d), color = colors[c], alpha=0.6}):tween(
			{alpha={0.0,1.5}, color={colors.white, 0.8}}, "msTween")
		end
		if(histogramEnabled) then
			barIndex = math.floor(1+histogramBars*(-d+45)/90)
			if(kernelEnabled) then
				for i = 1, kernelWidth do
					barSelectN = _G["histBar"..tostring(math.max(barIndex-i, 1))]
					barSelectP = _G["histBar"..tostring(math.min(barIndex+i, histogramBars))]
					barSelectN:update({dy = barSelectN.dy + 3*kernel[kernelCenter-i]}):activate(kernel[kernelCenter-i])
					barSelectP:update({dy = barSelectP.dy + 3*kernel[kernelCenter+i]}):activate(kernel[kernelCenter+i])
			end end
			barSelect = _G["histBar"..tostring(barIndex)]
			barSelect:update({dy = barSelect.dy + 3*kernelAmplitude}):activate() -- (-45, 45) into {1, ..., 15}
		end
end end

function opponentNoteHit(id, noteData, noteType, isSustain)
		for k, v in pairs(noteDirections) do
			if noteData+1 == k then
				if(eDiamondButtonsEnabled) then
					_G[opponentDiamondPrefix.."Diamond"..v[1].."Button"]:activate(opponentDiamondPrefix..v[2].."ButtonBeat", 3) end
				if(eDiamondArrowsEnabled) then
					_G[opponentDiamondPrefix.."Diamond"..v[1].."Arrow"]:activate(opponentDiamondPrefix..v[2].."ArrowBeat", 6) end
				if(eDiamondProxyEnabled) then
					_G[opponentDiamondPrefix.."Diamond"..v[1].."Proxy"]:activate(opponentDiamondPrefix..v[2].."ProxyBeat", 6) end
				return(noteData)
end end end