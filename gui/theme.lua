local modulepath = (...):gsub('[^.]+$', '')
local moduledir = modulepath:gsub("%.", "/")

local patch = require(modulepath .. "patch")
--local utf8 = require("utf8")

local min, max = math.min, math.max
local floor, abs = math.floor, math.abs
local function clamp(x, a, b) return min(max(x, a), b) end


local lg = love.graphics

local lgprint = function(text, x, y, ...)
	return lg.print(text, floor(x + .5), floor(y + .5), ...)
end
local lgprintf = function(text, x, y, ...)
	return lg.printf(text, floor(x + .5), floor(y + .5), ...)
end

local THEME_DRAW = {}

local THEME_COLORS = {
	text = {0, 0, 0, 1},
	back = {0.82, 0.80, 0.78, 1},
	active = {0.20, 0.55, 0.80, 1},
	activetext = {0.9, 0.9, 0.9, 1},
	
	tooltip = {0.95, 0.95, 0.85, 1},
}

local image2 = lg.newImage(moduledir.."default.png")
image2:setFilter("nearest")

local themefont = lg.newFont("assets/univga16.ttf", 16)
--print(themefont:getBaseline(), themefont:getAscent())


patch = patch.new(image2)

local THEME_FRAME = patch:new_3x3(0,0, 15,15, 4,4)
local THEME_BUTTON = patch:new_3x3(16,0, 15,15, 4,4)
local THEME_BUTTON_ON = patch:new_3x3(32,0, 15,15, 4,4)
local THEME_INPUT = patch:new_3x3(48,0, 15,15, 4,4)
local THEME_GROUP = patch:new_3x3(64,0, 15,15, 4,4)
--local THEME_INSET = patch:new_3x3(40,0, 7,7, 2,2)
--local THEME_INSET2 = patch:new_3x3(48,0, 7,7, 2,2)

local THEME_LINE_H = patch:new_3x1(0,16, 15,4, 3)
local THEME_LINE_V = patch:new_1x3(16,16, 4,15, 3)

local THEME_KNOB = patch:new_1x1(32, 16, 13, 13)

local THEME = {
	draw = THEME_DRAW,
	
	colors = THEME_COLORS,
	unit = 24, padw = 3, padh = 3,
	tipfont = lg.newFont(10),
	font = themefont,
	--drawicon = drawicon,
}



THEME_DRAW["tip"] = function(e)
	local gui = e.gui
	local theme = e:gettheme()
	local tipfont = theme.tipfont
	local padw, padh = e.padw, e.padh
	local tipw, tiph = tipfont:getWidth(e.tip), tipfont:getHeight()
	local scrw, scrh = lg.getDimensions()
	local posx, posy = e.absx, e.absy
	local x, y = posx + e.posw, posy - tiph - 3 * padh
	lg.setFont(tipfont) -- use the default font
	lg.setColor(theme.colors.tooltip)
	lg.rectangle("fill", x, y, tipw + 2 * padw, tiph + 2 * padh)
	lg.setColor(theme.colors.text)
	lgprint(e.tip, x + padw, y + padh)
end

-- DRAW FUNCTIONS >>
THEME_DRAW["group"] = function(e)
	--local pos = e.pos
	local theme = e:gettheme()
	local colors = theme.colors
	local unit, padw, padh = e.unit, e.padw, e.padh
	local font = e.font
	local fh = font:getHeight()
	
	lg.setColor(1,1,1,1)
	local a = e.parent and THEME_GROUP or THEME_FRAME
	a:draw(e.absx, e.absy, e.posw, e.posh)
	
	if e.label then
		lg.setFont(font)
		local fw = font:getWidth(e.label)
		if e.drag then
			lg.setColor(colors.active)
			lg.rectangle("fill", e.absx + padw, e.absy + padh, e.posw - 2*padw, unit - 2*padh)
			lg.setColor(colors.activetext)
			lgprint(e.label, e.absx + (e.posw - fw) / 2, e.absy + (unit - fh) / 2)
		else
			lg.setColor(colors.text)
			lgprint(e.label, e.absx + (e.posw - fw) / 2, e.absy + padh * 0.5)
		end
	end
end

THEME_DRAW["collapsegroup"]  = THEME_DRAW["group"]


THEME_DRAW["text"] = function(e)
	--local pos = e.pos
	local theme = e:gettheme()
	local colors = theme.colors
	local unit = e.unit
	local font = e.font
	local fh = font:getHeight()
	
	lg.setColor(colors.text)
	lg.setFont(font)
	if e.autosize then
		lgprint(e.label, e.absx, e.absy + (fh - e.posh) / 2)
	else
		lgprintf(e.label, e.absx, e.absy + (fh - e.posh) / 2, e.posw, "left")
	end
end

THEME_DRAW["typetext"]  = THEME_DRAW["text"]

THEME_DRAW["image"] = function(e)
	--local pos = e.pos
	local theme = e:gettheme()
	local colors = theme.colors
	local unit = e.unit
	local font = e.font

	if e.image then e:drawimage() end
	if e.label then
		lg.setColor(colors.text)
		lg.setFont(font)
		lgprint(e.label, e.absx + (e.posw - font:getWidth(e.label)) / 2,
		        e.absy + e.posh + (unit - font:getHeight()) / 2)
	end
end

THEME_DRAW["button"] = function(e)
	--local pos = e.pos
	local theme = e:gettheme()
	local colors = theme.colors
	local unit = e.unit
	local padw, padh = e.padw, e.padh
	local font = e.font

	local posx, posy = e.absx, e.absy
	
	local light, isdown = nil, 0
	if e == e.gui.hoverelement then light = 1.1 end

	lg.setColor(1,1,1,1)
	if (e.value and e.parent and e.parent.value == e.value) or e == e.gui.presselement then
		THEME_BUTTON_ON:draw(posx, posy, e.posw, e.posh)
		isdown = 1
	else
		THEME_BUTTON:draw(posx, posy, e.posw, e.posh, light)
	end
	
	
	if e.image and e.label then
		
		local iw, ih = e.image:getDimensions()
		local sx = min(1, (e.posh - 2 * padh) / iw)
		iw, ih = sx * iw, sx * ih
		
		local labelw, labelh = font:getWidth(e.label), font:getHeight()
		local x = posx + (e.posw - iw - padw - labelw) / 2
		
		lg.setColor(1,1,1,1)
		lg.draw(e.image, x, posy + (e.posh - ih) / 2, 0, sx, sx)
		
		lg.setColor(colors.text)
		lg.setFont(font)
		lgprint(e.label, x + padw + iw, posy + (e.posh - labelh) / 2 + isdown)
	elseif e.image then

		
	elseif e.label then
		lg.setColor(colors.text)
		lg.setFont(font)
		local labelw, labelh = font:getWidth(e.label), font:getHeight()
		if e.image then
			lgprint(e.label, posx + (e.posw - labelw) / 2, posy + (e.posh - labelh) / 2)
		else
			lgprint(e.label, posx + (e.posw - labelw) / 2, posy + (e.posh - labelh) / 2 + isdown)
		end
	end
end

THEME_DRAW["imagebutton"] = THEME_DRAW["button"]
THEME_DRAW["option"] = THEME_DRAW["button"]


THEME_DRAW["checkbox"] = function(e)
	--local pos = e.pos
	local theme = e:gettheme()
	local colors = theme.colors
	local unit = e.unit
	local font = e.font

	lg.setColor(1,1,1,1)
	THEME_INPUT:draw(e.absx, e.absy, e.posw, e.posh)
	if e.value then
		lg.setColor(colors.active)
		lg.rectangle("fill", e.absx + e.posw / 4, e.absy + e.posh / 4,
			e.posw / 2, e.posh / 2)
	end
	if e.label then
		lg.setColor(colors.text)
		lg.setFont(font)
		lgprint(e.label, e.absx + e.posw + unit / 2,
			e.absy + (e.posh - font:getHeight()) / 2)
	end
end


THEME_DRAW["input"] = function(e)
	--local pos = e.pos
	local theme = e:gettheme()
	local colors = theme.colors
	local unit = e.unit
	local font = e.font

	lg.setColor(1,1,1,1)
	THEME_INPUT:draw(e.absx, e.absy, e.posw, e.posh)
	-- Margin of edit box is unit/4 on each side, so total margin is unit/2
	local editw = e.posw - unit / 2
	if editw >= 1 then -- won"t be visible otherwise and we need room for the cursor
		-- We don"t want to undo the current scissor, to avoid printing text where it shouldn"t be
		-- (e.g. partially visible edit box inside a viewport) so we clip the current scissor.
		local sx, sy, sw, sh = lg.getScissor()
		lg.intersectScissor(e.absx + unit / 4, e.absy, editw, e.posh)
		lg.setColor(colors.text)
		local str, cursorx = e.value, e.cursorx
		if e.ispassword then
			str = string.rep(e.passwordchar, e.valuelen)
			cursorx = font:getWidth(str:sub(1, e.cursor - 1))
		end
		-- cursorx is the position relative to the start of the edit box
		-- (add e.absx + unit/4 to obtain the screen X coordinate)
		local cursorx = e.textorigin + cursorx
		-- adjust text origin so that the cursor is always within the edit box
		if cursorx < 0 then
			e.textorigin = min(0, e.textorigin - cursorx)
			cursorx = 0
		end
		if cursorx > editw - 1 then
			e.textorigin = min(0, e.textorigin - cursorx + editw - 1)
			cursorx = editw - 1
		end
		-- print the whole text and let the scissor do the clipping
		lgprint(str, e.absx + unit / 4 + e.textorigin,
			e.absy + (e.posh - font:getHeight()) / 2)
		if e == e.gui.focuselement and e.cursorlife < 0.5 then
			lg.rectangle("fill", e.absx + unit / 4 + cursorx,
				e.absy + unit / 8, 1, e.posh - unit / 4)
		end
		-- restore current scissor
		lg.setScissor(sx, sy, sw, sh)
	end
	if e.label then
		lg.setColor(colors.text)
		lgprint(e.label, e.absx - font:getWidth(e.label),
			e.absy + (e.posh - font:getHeight()) / 2)
	end
end

THEME_DRAW["scroll"] = function(e)
	--local pos = e.pos
	local theme = e:gettheme()
	local colors = theme.colors
	local unit = e.unit
	local font = e.font

	lg.setColor(1,1,1,1)
	local light
	local vertical = e.isvertical
	local gui = e.gui
	--local theme = gui.theme
	
	if e == gui.presselement then
		light = 1.15
	elseif e == gui.hoverelement then
		light = 1.05
	--else
	--	light = 0.0
	end
	
	local hs = max(13, e.handlesize)
	local rpos = (e.values.current - e.values.min) / (e.values.max - e.values.min)
	
	lg.setColor(1,1,1,1)
	
	if vertical then
		THEME_LINE_V:draw(e.absx, e.absy + 2, e.posw, e.posh - 4)
		rpos = floor(e.absy + (e.posh - hs) * rpos)
		if hs > 13 then
			THEME_BUTTON:draw(e.absx, clamp(rpos, e.absy, e.absy + e.posh - hs), e.posw, hs, light)
		else -- hs == 13
			THEME_KNOB:draw(e.absx + floor((e.posw - hs) / 2), clamp(rpos, e.absy, e.absy + e.posh - hs), hs, hs, light)
		end
		if e.label then
			lg.setColor(colors.text)
			lg.setFont(font)
			local labelw, labelh = font:getWidth(e.label), font:getHeight()
			lgprint(e.label, e.absx + (e.posw - labelw) / 2, e.absy + e.posh + (unit - labelh) / 2)
		end
	else
		THEME_LINE_H:draw(e.absx + 2, e.absy, e.posw - 4, e.posh)
		rpos = floor(e.absx + (e.posw - hs) * rpos)
		if hs > 13 then
			THEME_BUTTON:draw(clamp(rpos, e.absx, e.absx + e.posw - hs), e.absy, hs, e.posh, light)
		else
			THEME_KNOB:draw(clamp(rpos, e.absx, e.absx + e.posw - hs), e.absy + floor(e.posh - hs), hs, hs, light)
		end
		if e.label then
			lg.setColor(colors.text)
			lg.setFont(font)
			local labelw, labelh = font:getWidth(e.label), font:getHeight()
			lgprint(e.label, e.absx - labelw - 2, e.absy + 0.5 * (e.posh - labelh))
		end
	end
end

THEME_DRAW["scrollgroup"] = function(e)
	--local pos = e.pos
	local theme = e:gettheme()
	local colors = theme.colors
	local unit = e.unit
	local font = e.font

	if e.label then
		lg.setColor(colors.text)
		lgprint(e.label, e.absx + (e.posw - font:getWidth(e.label)) / 2,
			e.absy + (unit - font:getHeight()) / 2)
	end
end

-- DRAW FUNCTIONS <<

return THEME
