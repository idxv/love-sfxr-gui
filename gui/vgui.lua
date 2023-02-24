-- Copyright (C) 2022 idxv

-- This software is provided 'as-is', without any express or implied warranty. In no event will the authors be held liable for any damages arising from the use of this software.
-- Permission is granted to anyone to use this software for any purpose, including commercial applications, and to alter it and redistribute it freely, subject to the following restrictions:
-- 1. The origin of this software must not be misrepresented; you must not claim that you wrote the original software. If you use this software in a product, an acknowledgment in the product documentation would be appreciated but is not required.
-- 2. Altered source versions must be plainly marked as such, and must not be misrepresented as being the original software.
-- 3. This notice may not be removed or altered from any source distribution.

local modulepath = (...):gsub('%.[^.]+$', '')

local loadsubmodule = function(GUI, name)
	local t = require(modulepath .. "." .. name)
	if t.register then
		t.register(GUI, t.name)
		return
	end
	
	for i, v in ipairs(t) do
		v.register(GUI, v.name)
	end
end

local utf8 = require("utf8")

local min, max = math.min, math.max
local floor, abs = math.floor, math.abs

local function clamp(x, a, b) return min(max(x, a), b) end

local function round(x)
	if x >= 0 then return floor(x + 0.5) end
	return -floor(-x + 0.5)
end

local function roundn(x, n)
	if x >= 0 then return floor(x * n + 0.5) / n end
	return -floor(-x * n + 0.5) / n
end

local function _clone(t, cache)
	if type(t) ~= "table" then return t end
	if cache[t] then return cache[t] end
	local ct = {}
	cache[t] = ct
	for k, v in pairs(t) do
		ct[_clone(k, cache)] = _clone(v, cache)
	end
	return setmetatable(ct, getmetatable(t))
end
local clone = function(t) return _clone(t, {}) end

local getindex = function(t, o)
	for i, v in ipairs(t) do if v == o then return i end end
end


local withinrect = function(px, py, x, y, w, h)
	return px >= x and px < x + w and py >= y and py < y + h
end

local withinellipse = function(px, py, cx, cy, r1, r2)
	return (px - cx)^2 / r1^2 + (py - cy)^2 / r2^2 < 1
end


local lg = love.graphics


local theme = require(modulepath .. ".theme")

local GUI = {}

GUI.new = function()
	local newgui = {
		theme = theme,
		elements = {}, -- root elements
		hoverelement = nil,-- current hover element
		focuselement = nil,-- current focus element
		presselement = nil,-- current press element
	}
	
	return setmetatable(newgui, {__index = GUI})
end

-- assumes parent area contains children
local function elementhover(gui, e, mx, my)
	if not e.display then return end
	
	for i = 1, e.numoverlay do
		if elementhover(gui, e.children[i], mx, my) then
			return gui.hoverelement
		end
	end
	
	if not e:containspoint(mx, my) then return end
	
	for i = #e.children, e.numoverlay + 1, -1 do
		if elementhover(gui, e.children[i], mx, my) then
			return gui.hoverelement
		end
	end
	
	gui.hoverelement = e
	return e
end

local function elementbucket(gui, e, mx, my)
	if not e.display then return end
	if not e:containspoint(mx, my) then return end
	
	for i = #e.children, e.numoverlay + 1, -1 do
		if elementbucket(gui, e.children[i], mx, my) then
			return gui.bucketelement
		end
	end
	if e ~= gui.presselement then
		gui.bucketelement = e
		return e
	end
end


local function eupdatef(gui, e, dt)
	if not e.display or not e.update then return end
	
	if e.updateinterval then
		e.dt = e.dt + dt
		if e.dt < e.updateinterval then return end
		e.dt = 0
	end
	e:update(dt)
end

local function elementupdate(gui, e, dt)
	eupdatef(gui, e, dt)
	for i, child in ipairs(e.children) do elementupdate(gui, child, dt) end
end


local function eupdateposf(gui, e)
	local parent, x, y = e.parent, e.posx, e.posy
	if not parent then e.absx, e.absy = x, y return end
	x, y = x + parent.absx, y + parent.absy
	if parent.classname == "scrollgroup" and e ~= parent.scrollv and e ~= parent.scrollh then
		if parent.scrollv then y = y - parent.scrollv.values.current end
		if parent.scrollh then x = x - parent.scrollh.values.current end
	end
	e.absx, e.absy = x, y
end
local function elementupdatepos(gui, e)
	eupdateposf(gui, e)
	for i, child in ipairs(e.children) do elementupdatepos(gui, child) end
end

GUI.elementupdatepos = elementupdatepos

GUI.updatepositions = function(gui)
	for i, child in ipairs(gui.elements) do
		elementupdatepos(gui, child)
	end
end

local genericdrag = function(gui, e, mx, my)
	local parent = e.parent
	if parent then
		e.posx = clamp(mx - e.offsetx - parent.absx, 0, parent.posw - e.posw)
		e.posy = clamp(my - e.offsety - parent.absy, 0, parent.posh - e.posh)
	else
		e.posx = mx - e.offsetx
		e.posy = my - e.offsety
	end
	elementupdatepos(gui, e)
end

GUI.update = function(gui, dt)
	local mousex, mousey = gui:getmouse()
	local hoverelement = gui.hoverelement
	gui.hoverelement = nil
	
	local pe = gui.presselement
	if pe and pe.drag then
		if love.mouse.isDown(1) then
			if type(pe.drag) == "function" then pe:drag(mousex, mousey)
			else genericdrag(gui, pe, mousex, mousey) end
		end
	end
	
	for i = #gui.elements, 1, -1 do
		if elementhover(gui, gui.elements[i], mousex, mousey) then break end
	end
	for i = #gui.elements, 1, -1 do
		elementupdate(gui, gui.elements[i], dt)
	end
	
	if gui.hoverelement ~= hoverelement then
		if hoverelement and hoverelement.leave then hoverelement:leave() end
		if gui.hoverelement and gui.hoverelement.enter then gui.hoverelement:enter() end
	end
end

local function elementdraw(gui, element)
	if not element.display then return end
	
	element:draw()
	
	local scissor = element.scissor
	if scissor then
		sx, sy, sw, sh = lg.getScissor()
		lg.intersectScissor(element.absx + scissor.x, element.absy + scissor.y, scissor.w, scissor.h)
	end
	
	for i = element.numoverlay + 1, #element.children do
		elementdraw(gui, element.children[i])
	end
	
	if scissor then lg.setScissor(sx, sy, sw, sh) end
	
	for i = 1, element.numoverlay do
		elementdraw(gui, element.children[i])
	end
end

GUI.draw = function(gui)
	lg.push("all")
	lg.setLineWidth(1)
	lg.setColor(1,1,1,1)

	for i, element in ipairs(gui.elements) do
		elementdraw(gui, element)
	end
	local element = gui.hoverelement
	if element and element.tip then
		gui.theme.draw.tip(element)
	end
	lg.pop()
end

GUI.mousepress = function(gui, x, y, button, istouch, presscount)
	if not presscount then presscount = 1
	elseif presscount > 2 then return end
	
	local he = gui.hoverelement
	
	if gui.focuselement and gui.focuselement ~= he then gui:remfocus() end
	
	if button == 1 then gui.presselement = he end
	if not he then return end
	
	he:getparent():movetotop()
	
	if button == 1 then
		if he.drag then
			--gui.dragelement = he
			he.offsetx, he.offsety = x - he.absx, y - he.absy
		end
		if presscount == 1 then
			if he.press then he:press(x, y, button) end
		elseif he.dblclick then
			he:dblclick(x, y, button)
		end
	elseif button == 2 and he.rclick then
		he:rclick(x, y)
	end
end

GUI.mouserelease = function(gui, x, y, button, istouch, presscount)
	local pe = gui.presselement
	if not pe then return end
	
	if button == 1 then gui.presselement = nil end
	
	if button == 1 and gui.hoverelement and gui.hoverelement == pe then
		if pe.click then pe:click(x, y) end
	end
	
end

GUI.mousewheel = function(gui, x, y)
	local element = gui.hoverelement
	if y == 0 or not element then return end
	local action = y > 0 and element.wheelup or element.wheeldown
	if not action then return end
	action(element, gui.getmouse())
end

GUI.keypress = function(gui, key, scancode, isrepeat)
	local element = gui.focuselement
	if not element or (isrepeat and not element.allowkeyrepeat) then return end
	
	if key == "return" and element.done then element:done() end
	element = gui.focuselement
	if element and element.keypress then element:keypress(key) end
end

GUI.textinput = function(gui, key)
	if gui.focuselement and gui.focuselement.textinput then gui.focuselement:textinput(key) end
end

GUI.getmouse = function()
	return love.mouse.getPosition()
end

GUI.add = function(gui, element, parent)
	gui:rem(element)
	
	if parent then
		element.parent = parent
		table.insert(parent.children, element)
		element:updatepos()
		return element
	end
	
	table.insert(gui.elements, element)
	element:updatepos()
	return element
end

local function _elementhas(gui, element, value)
	if element == value then return element end
	for i, child in ipairs(element.children) do
		if _elementhas(gui, element, value) then return element end
	end
end
local elementhas = function(gui, element, value)
	if not element or not value then return end
	return _elementhas(gui, element, value)
end

GUI.rem = function(gui, element)
	local parent, list, index = element.parent
	if parent then
		list = parent.children; index = getindex(list, element)
		if not index then error("not a children of its parent") end
		element.parent = nil
	else
		list = gui.elements; index = getindex(gui.elements, element)
		if not index then return end -- double remove?
	end
	table.remove(list, index)
	if elementhas(gui, element, gui.hoverelement) then gui.hoverelement = nil end
	if elementhas(gui, element, gui.presselement) then gui.presselement = nil end
	if elementhas(gui, element, gui.focuselement) then gui:remfocus() end
	
	element.posx, element.posy = 0, 0
	return element
end


GUI.remfocus = function(gui)
	local element = gui.focuselement
	if not element then return end
	gui.focuselement = nil
	if element.unfocus then element:unfocus() end
end

GUI.setfocus = function(gui, element)
	if not element then print("warning: setfocus to nil") return end
	if gui.focuselement == element then return end
	gui.focuselement = element
	if element.focus then element:focus() end
end

local elementmethods = {
	setimage = function(e, image, quad, transform)
		e.image = image
		e.imagequad = quad
		e.imagetransform = transform
	end,

	drawimage = function(e)
		local r, g, b, a = lg.getColor()
		lg.setColor(1, 1, 1, 1)
		lg.draw(e.image, (e.absx + (e.posw / 2)) - (e.image:getWidth()) / 2,
			e.absy + e.posh / 2 - e.image:getHeight() / 2)
		lg.setColor(r, g, b, a)
	end,

	setfont = function(e, font)
		if not font then
			font = e.parent and e.parent.font or e.gui.theme.font
		end
		e.font = font
		
		if e.autosize then
			e.posw = font:getWidth(e.label) + 2 * e.padw
			e.posh = font:getHeight()-- + 2 * e.padh
		end
	end,
	
	updatepos = function(e)
		elementupdatepos(e.gui, element)
	end,
	
	containspoint = function(e, x, y)
		return withinrect(x, y, e.absx, e.absy, e.posw, e.posh)
	end,

	getparent = function(e)
		while e.parent do e = e.parent end
		return e
	end,
	
	gettheme = function(e)
		while not e.theme and e.parent do
			e = e.parent
		end
		return e.theme or e.gui.theme
	end,
	
	settheme = function(e, theme)
		local parent = e.parent
		
		while not parent.theme and parent.parent do
			parent = parent.parent
		end
		
		e.theme  = setmetatable(theme, {__index = parent and parent.theme or e.gui.theme})
	end,
	
	getmaxw = function(e)
		local maxw = 0
		for i = e.numoverlay + 1, #e.children do
			local c = e.children[i]
			maxw = max(maxw, c.posx + c.posw)
		end
		return maxw
	end,

	getmaxh = function(e)
		local maxh = 0
		for i = e.numoverlay + 1, #e.children do
			local c = e.children[i]
			maxh = max(maxh, c.posy + c.posh)
		end
		return maxh
	end,

	addchild = function(e, child)
		e.gui:rem(child)
		e.gui:add(child, e)
		return child
	end,

	remchild = function(e, child)
		if child.parent ~= e then error("cannot remove from non parent") end
		return e.gui:rem(child)
	end,

	--replace = function(e, replacement)
	--	e.gui.elements[getindex(e.gui.elements, e)] = replacement
	--	return replacement
	--end,

	getlevel = function(e)
		local elements = e.parent and e.parent.children or e.gui.elements
		return getindex(elements, e)
	end,

	setlevel = function(e, level)
		local elements = e.parent and e.parent.children or e.gui.elements
		local index = getindex(elements, e)
		table.insert(elements, level, table.remove(elements, index or -1))
	end,

	movetotop = function(e) -- currently only called for gui.elements so parent == nil
		if e.isoverlay then return end
		local elements = e.parent and e.parent.children or e.gui.elements
		table.insert(elements, table.remove(elements, getindex(elements, e) or -1))
	end,

	visible = function(e, display)
		e.display = display
		for i, child in pairs(e.children) do child:visible(display) end
	end,

	setfocus = function(e, focus)
		if focus == nil or focus then e.gui:setfocus(e) return end
		if focus == false and e.gui.focuselement == e then e.gui:remfocus() end
	end,

	draw = function(e)
		e.gui.theme.draw[e.classname](e)
	end,
}
GUI.elementmethods = elementmethods

local newelement = function(class, gui, label, pos, parent, ...)
	assert(gui[class.classname], "unknown gui class")
	assert(type(parent) == "table" and parent.gui == gui or parent == nil, "invalid parent")
	
	local style, elements = gui.theme, gui.elements
	if parent then style, elements = parent, parent.children end
	
	local unit = style.unit
	local posx, posy, posw, posh
	if pos then
		if pos[1] then posx, posy, posw, posh = pos[1], pos[2], pos[3], pos[4]
		else posx, posy, posw, posh = pos.x, pos.y, pos.w, pos.h end
		
		posx, posy, posw, posh = posx or 0, posy or 0, posw or unit, posh or unit
	else
		posx, posy, posw, posh = 0, 0, unit, unit
	end
	
	local element = {label = label, display = true, dt = 0,
		posx = posx, posy = posy, posw = posw, posh = posh,
		absx = 0, absy = 0, offsetx = 0, offsety = 0, numoverlay = 0,
		unit = style.unit, padw = style.padw, padh = style.padh, font = style.font,
		parent = parent, children = {}, gui = gui}
	

	setmetatable(element, class.classobjectmeta)
	if element.init then element:init(...) end
	table.insert(elements, element)
	return element
end

GUI.newtype = function(classname, class, superclassname)
	class.classname = classname
	class.classobjectmeta = { -- used by GUI.element()
		__index = class,
		__tostring = function(e) return ("%s (%s)"):format(e.classname, e:getlevel() or "detached") end
	}
	if not class.load then class.load = newelement end
	if superclassname then
		local super = GUI[superclassname]
		class.classsuper = super
		for k, v in pairs(super) do
			if class[k] == nil then class[k] = v end
		end
	end

	setmetatable(class, {__index = elementmethods, __call = class.load})
	GUI[classname] = class
end

-- elements
GUI.newtype("group", {
	getminheight = function(e)
		if e.drag then return e.unit end
		if e.label then return e.font:getHeight() + e.padh end
		return e.padh
	end,
	
	verticaltile = function(e)
		local children = e.children
		--local theme = e:gettheme()
		local pad = e.padh
		local y = e:getminheight()
		for i = e.numoverlay + 1, #children do
			local c = children[i]
			c.posy = y
			y = y + c.posh + pad
		end
		
		if e.updateheight then e.updateheight(e, y)
		else e.posh = y end
	end,
})

local cgpcontrolclick = function(e) e.parent:toggle() end

GUI.newtype("collapsegroup", {
	init = function(e)
		e.view = true
		e.origh = e.posh
		local bu = math.max(e.unit - 2 * e.padh, 10)
		e.control = e.gui:button(nil, {e.posw - bu - e.padw, e.padh, bu, bu}, e)
		e.control.unit = bu
		e.control.click = cgpcontrolclick
		e.control.isoverlay = true
		e.numoverlay = 1
	end,
	toggle = function(e)
		e.view = not e.view
		e.posh = e.view and e.origh or e.unit
		for i, child in ipairs(e.children) do
			if child ~= e.control then child:visible(e.view) end
		end
	end,
	updateheight = function(e, height)
		e.origh = height
		if e.view then e.posh = height end
	end,
}, "group")

GUI.newtype("text", {
	init = function(e, autosize)
		if autosize then
			e.posw = e.font:getWidth(e.label) + 2 * e.padw
			e.autosize = autosize
		end
		e:setfont(e.font)
	end,
	setfont = function(e, font)
		elementmethods.setfont(e, font)
		if not e.autosize then -- height needs adjustment regardless
			local width, lines = e.font:getWrap(e.label, e.posw - 2 * e.padw)
			if type(lines) == "table" then lines = #lines end
			lines = max(lines, 1)
			e.posh = e.font:getHeight() * lines -- + e.padh
		end
	end,
})

GUI.newtype("typetext", {
	init = function(e, autosize)
		e.classsuper.init(e, autosize)
		e.text = e.label
		e.textcursor = 1
		e.textoffset = 1
		e.textlen = utf.len(e.label)
		e.updateinterval = 0.1
		e.label = ""
	end,
	update = function(e, dt)
		if e.textcursor > e.textlen then return end
		e.textcursor = min(e.textcursor + 1, e.textlen + 1)
		e.textoffset = utf8.offset(e.text, 2, e.textoffset)
		e.label = e.values.text:sub(1, e.textoffset - 1)
	end
}, "text")

GUI.newtype("image", {
	init = function(e, image) e:setimage(image) end,
	setimage = function(e, image)
		elementmethods.setimage(e, image)
		if e.image then
			e.posw = e.image:getWidth()
			e.posh = e.image:getHeight()
		end
	end,
})

GUI.newtype("button", {
	init = function(e, autosize)
		if autosize then e.autosize = autosize end
	end,
})

GUI.newtype("imagebutton", {
	init = function(e, image) e:setimage(image) end,
}, "button")

GUI.newtype("option", {
	init = function(e, value)
		e.autosize = true
		e.value = value
	end,
	click = function(e)
		if not e.parent then return end
		e.parent.value = e.value
	end,
}, "button")

GUI.newtype("checkbox", {
	init = function(e, value) e.value = value and true or false end,
	click = function(e) e.value = not e.value end,
})

GUI.newtype("input", {
	init = function(e, value, ispassword)
		e.value = (value and tostring(value)) or ""
		e.valuelen = utf8.len(e.value)
		e.cursor = 0 -- utf8 char index
		--e.cursoroffset = 1 -- corresponding byte offset
		--e.cursorx = 0 -- corresponding cursorx
		e:cursormend()
		e.textorigin = 0
		e.cursorlife = 0
		e.ispassword = ispassword or false
		e.passwordchar = "*"
	end,
	update = function(e, dt)
		if e.cursor > e.valuelen then e.cursor = e.valuelen + 1 end
		if e.gui.focuselement == e then
			if e.cursorlife >= 1 then e.cursorlife = 0
			else e.cursorlife = e.cursorlife + dt end
		end
	end,
	
	click = function(e) e:setfocus() end,
	done = function(e) e:setfocus(false) end,
	
	cursorxupdate = function(e) -- unsafe
		e.cursorx = e.font:getWidth(e.value:sub(1, e.cursoroffset - 1))
	end,
	
	cursormleft = function(e)
		if e.cursor <= 1 then return end
		e.cursor = e.cursor - 1
		e.cursoroffset = utf8.offset(e.value, -1, e.cursoroffset)
		e:cursorxupdate()
		return true
	end,
	cursormright = function(e)
		if e.cursor > e.valuelen then return end
		e.cursor = e.cursor + 1
		e.cursoroffset = utf8.offset(e.value, 2, e.cursoroffset)
		e:cursorxupdate()
		return true
	end,
	cursormhome = function(e)
		e.cursor = 1
		e.cursoroffset = 1
		e.cursorx = 0
	end,
	cursormend = function(e)
		if e.cursor > e.valuelen then return end
		e.cursor = e.valuelen + 1
		e.cursoroffset = #e.value + 1
		e:cursorxupdate()
	end,
	cursormdelete = function(e)
		if e.cursor > e.valuelen then return end
		local offset = e.cursoroffset
		local nextoffset = utf8.offset(e.value, 2, offset)
		
		if e.cursor == 1 then e.value = e.value:sub(nextoffset)
		elseif e.cursor == e.valuelen then e.value = e.value:sub(1, offset - 1)
		else e.value = e.value:sub(1, offset - 1) .. e.value:sub(nextoffset) end
		e.valuelen = e.valuelen - 1
		return true
	end,
	
	keypress = function(e, key)
		local savecursorlife = e.cursorlife
		e.cursorlife = 0

		if key == "backspace" then
			if e:cursormleft() then e:cursormdelete() end
		elseif key == "delete" then e:cursormdelete()
		elseif key == "left" then e:cursormleft()
		elseif key == "right" then e:cursormright()
		elseif key == "home" then e:cursormhome()
		elseif key == "end" then e:cursormend()
		elseif key == "tab" then
			if e.next then e.next:setfocus() end
		elseif key == "escape" then e:setfocus(false)
		else
			e.cursorlife = savecursorlife
		end
	end,

	textinput = function(e, key)
		
		if e.cursor == 1 then e.value = key .. e.value
		elseif e.cursor > e.valuelen then e.value = e.value .. key
		else
			e.value = e.value:sub(1, e.cursoroffset - 1) .. key .. e.value:sub(e.cursoroffset)
		end
		e.valuelen = e.valuelen + utf8.len(key)
		
		e.cursor = e.cursor + utf8.len(key)
		e.cursoroffset = e.cursoroffset + #key
		e:cursorxupdate()
		
		-- reset blink timer
		e.cursorlife = 0
	end,
})


loadsubmodule(GUI, "scroll")


return GUI
