local lg = love.graphics

local min, max = math.min, math.max
local floor, abs = math.floor, math.abs
local function clamp(x, a, b) return min(max(x, a), b) end

local setscrollvalues = function(sv)
	local l, r, v, step
	if sv == nil then
		return {min = 0, max = 10, current = 0, step = 1}
	else
		if sv[1] then
			l, r, v, step = sv[1], sv[2], sv[3], sv[4]
		else
			l, r, v, step = sv.min, sv.max, sv.current, sv.step
		end
	end
	
	if not (l and r) then error("missing right or left values") end
	
	local minv, maxv = min(l, r), max(l, r)
	step = step and clamp(abs(step), 0, maxv - minv) or (maxv - minv) / 10
	if l ~= minv then step = -step end -- subtracted as it goes from l to r
	v = v and clamp(v, minv, maxv) or l
	
	return {min = minv, max = maxv, current = v, step = step}
end

local scrollvaluesstep = function(sv, n)
	sv.current = clamp(sv.current + n * sv.step, sv.min, sv.max)
end

local function registerscroll(GUI, typename)

--local setscrollvalues = GUI.setscrollvalues

GUI.newtype(typename, {
	init = function(e, values, isvertical)
		e.values = setscrollvalues(values)
		e.isvertical = isvertical and true or false
		e.handlesize = floor(e.unit / 2)
	end,
	--update = function(e, dt)
	--	local x, y = e.gui:getmouse()
	--	if e.withinrect(x, y, e.pos) and not e.gui.dragelement then e.gui.hoverelement = e end
	--end,
	sethandlesize = function(e, value)
		if type(value) == "number" then
			e.autohandlesize = nil
			e.handlesize = value
			return
		end
		if value == "auto" then e.autohandlesize = true
		elseif value == nil and not e.autohandlesize or value ~= nil then return end
		
		local sv = e.values
		local hs = e.handlesize or e.unit
		
		if e.isvertical then
			local h = e.parent and e.parent.posh or e.posh
			hs = clamp(e.posh * h / (sv.max - sv.min + h), e.unit / 4, e.posh)
		else
			local w = e.parent and e.parent.posw or e.posw
			hs = clamp(e.posw * w / (sv.max - sv.min + w), e.unit / 4, e.posw)
		end
		e.handlesize = floor(hs)
	end,
	step = function(e, n)
		local sv = e.values
		local oldvalue = sv.current
		sv.current = clamp(sv.current + n * sv.step, sv.min, sv.max)
		
		if e.onchange and sv.current ~= oldvalue then e:onchange() end
	end,
	drag = function(e, x, y)
		local hs, sv = e.handlesize, e.values
		local slen, moff, rmp -- scrollable len, mouse offset, relative mouse pos
		
		if e.isvertical then
			slen, moff, rmp = e.posh - hs, e.offsety, y - e.absy
		else
			slen, moff, rmp = e.posw - hs, e.offsetx, x - e.absx
		end
				
		if slen <= 0 then return end
		local oldvalue = sv.current
		
		local spos = (slen * (sv.current - sv.min)) / (sv.max - sv.min) -- current handle pos
		if moff >= spos and moff < spos + hs then
			local dx, dy = x - e.absx - e.offsetx,  y - e.absy - e.offsety
			if dx == 0 and dy == 0 then return end
			e.offsetx, e.offsety = e.offsetx + dx, e.offsety + dy
			
			local delta = e.isvertical and dy or dx
			sv.current = clamp(sv.current + ((sv.max - sv.min) * delta) / slen, sv.min, sv.max)
		else
			local delta = (rmp - floor(hs / 2)) / slen
			sv.current = clamp(sv.min + (sv.max - sv.min) * delta, sv.min, sv.max)
		end

		if e.onchange and sv.current ~= oldvalue then e:onchange() end
	end,
	wheelup = function(e) e:step(1) end,
	wheeldown = function(e) e:step(-1) end,
	keypress = function(e, key)
		if key == "up" or key == "right" then e:step(1) return end
		if key == "down" or key == "left" then e:step(-1) return end
		
		if key == "tab" then
			if e.next then e.next:setfocus() end
		elseif key == "escape" then
			e:setfocus(false)
		end
	end,
	--done = function(e) e:setfocus(false) end, -- on enter
	enter = function(e) if not e.gui.focuselement then e:setfocus() end end,
	leave = function(e) e:setfocus(false) end,
})
GUI[typename].rdrag = GUI[typename].drag

end -- registerscroll



local function registerscrollgroup(GUI, typename)

local scrollgponchange = function(e)
	e.gui:elementupdatepos(e.parent)
end

GUI.newtype(typename, {
	init = function(e, axis)
		local gui, unit = e.gui, e.unit
		local padw, padh = e.padw, e.padh
		local vertical, horizontal = (axis == "vertical"), (axis == "horizontal")
		if axis == "both" then vertical, horizontal = true, true end
		e.scissor = {x = 0, y = 0, w = e.posw, h = e.posh}
		
		local scrollv, scrollh
		
		if vertical then
			scrollv = gui:scroll(nil, {x = e.posw, y = 0, w = floor(unit/2), h = e.posh},
				e, {0, 0, 0, unit}, true)
			scrollv.onchange = scrollgponchange
			scrollv.isoverlay = true
			e.scrollv = scrollv
		end
			
		if horizontal then
			scrollh = gui:scroll(nil, {x = 0, y = e.posh, w = e.posw, h = floor(unit/2)},
				e, {0, 0, 0, unit}, false)
			scrollh.onchange = scrollgponchange
			scrollh.isoverlay = true
			e.scrollh = scrollh
		end
		
		if scrollv then e.posw = e.posw + scrollv.posw end
		if scrollh then e.posh = e.posh + scrollh.posh end
		e.numoverlay = #e.children
	end,
	updatecontent = function(e)
		if e.scrollh then
			e.scrollh.values.max = max(e:getmaxw() - e.posw, 0)
			e.scrollh:sethandlesize("auto")
		end
		if e.scrollv then
			e.scrollv.values.max = max(e:getmaxh() - e.posh, 0)
			e.scrollv:sethandlesize("auto")
		end
	end,
})

end -- registerscrollgroup

return {{name = "scroll", register = registerscroll},
	{name = "scrollgroup", register = registerscrollgroup}}