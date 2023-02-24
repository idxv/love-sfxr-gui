local modulepath = (...):gsub('%.[^.]+$', '')
local moduledir = modulepath:gsub("%.", "/")

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

local pixelcode = 
[[
	uniform float l;
	vec4 effect(vec4 color, Image tex, vec2 texture_coords, vec2 screen_coords)
	{
		vec4 texcolor = Texel(tex, texture_coords);
		texcolor.rgb += l;
		return texcolor * color;
	}
]]

local shader3x3 = love.graphics.newShader(pixelcode,
[[
	uniform float t[8]; // 0:x, 1:y, 2:w, 3:h, 4:bw, 5:bh, 6:cw, 7:ch
	vec4 position(mat4 transform_projection, vec4 pos)
	{
		pos.x = t[0] + sign(pos.x) * t[4] + step(0.5, pos.x) * (t[2] - t[4] - t[6]) + step(1, pos.x) * t[6];
		pos.y = t[1] + sign(pos.y) * t[5] + step(0.5, pos.y) * (t[3] - t[5] - t[7]) + step(1, pos.y) * t[7];
		return transform_projection * pos;
	}
]]
)

local draw3x3 = function(self, x, y, w, h, l)
	love.graphics.setShader(shader3x3)
	shader3x3:send("t", x, y, w, h, self.bw, self.bh, self.cw, self.ch)
	shader3x3:send("l", l or 0)
	love.graphics.draw(self.mesh)
	love.graphics.setShader()
end

local create3x3 = function(image, x0, y0, w, h, bw, bh, cw, ch)
	local W, H = image:getDimensions()
	if not cw then cw = bw end
	if not ch then ch = bh end
	
	local u0, u1, u2, u3 = x0 / W, (x0 + bw) / W, (x0 + w - cw) / W, (x0 + w) / W
	local v0, v1, v2, v3 = y0 / H, (y0 + bh) / H, (y0 + h - ch) / H, (y0 + h) / H
	
	local vertices = {
		{0, 0.0, u0, v0}, {0.2, 0.0, u1, v0}, {0.8, 0.0, u2, v0}, {1, 0.0, u3, v0}, 
		{0, 0.2, u0, v1}, {0.2, 0.2, u1, v1}, {0.8, 0.2, u2, v1}, {1, 0.2, u3, v1}, 
		{0, 0.8, u0, v2}, {0.2, 0.8, u1, v2}, {0.8, 0.8, u2, v2}, {1, 0.8, u3, v2}, 
		{0, 1.0, u0, v3}, {0.2, 1.0, u1, v3}, {0.8, 1.0, u2, v3}, {1, 1.0, u3, v3}, 
	}
	
	local tris, n, i = {}, 0, 1
	repeat
		tris[n + 1], tris[n + 2], tris[n + 3] = i, i + 1, i + 5
		tris[n + 4], tris[n + 5], tris[n + 6] = i, i + 5, i + 4
		n = n + 6; i = i + (i % 4 == 3 and 2 or 1)
		--if holed and i == 6 then i = 7 end
	until i > 11
	
	local mesh = love.graphics.newMesh(vertices, "triangles", static)
	mesh:setVertexMap(tris)
	mesh:setTexture(image)
	
	return {mesh = mesh, draw = draw3x3, bw = bw, bh = bh, cw = cw, ch = ch,
		_x = x0, _y = y0, _w = w, _h = h}
end

local shader3x1 = love.graphics.newShader(pixelcode,
[[
	uniform float t[4];
	vec4 position(mat4 transform_projection, vec4 pos)
	{
		pos.x = t[0] + sign(pos.x) * t[3] + step(0.5, pos.x) * (t[2] - 2 * t[3]) + step(1, pos.x) * t[3];
		pos.y += t[1];
		return transform_projection * pos;
	}
]]
)

local draw3x1 = function(self, x, y, w, h, l)
	love.graphics.setShader(shader3x1)
	shader3x1:send("t", x, y + (h - self.bh) / 2, w, self.bw)
	shader3x1:send("l", l or 0)
	love.graphics.draw(self.mesh)
	love.graphics.setShader()
end

local create3x1 = function(image, x0, y0, w, h, bw)
	local W, H = image:getDimensions()
	
	local u0, u1, u2, u3 = x0 / W, (x0 + bw) / W, (x0 + w - bw) / W, (x0 + w) / W
	local v0, v1 = y0 / H, (y0 + h) / H
	
	local vertices = {
		{0, 0, u0, v0}, {0.2, 0, u1, v0}, {0.8, 0, u2, v0}, {1, 0, u3, v0},
		{0, h, u0, v1}, {0.2, h, u1, v1}, {0.8, h, u2, v1}, {1, h, u3, v1},
	}
	
	local tris, n = {}, 0
	for i = 1, 3 do
		tris[n + 1], tris[n + 2], tris[n + 3] = i, i + 1, i + 5
		tris[n + 4], tris[n + 5], tris[n + 6] = i, i + 5, i + 4
		n = n + 6
	end
	
	local mesh = love.graphics.newMesh(vertices, "triangles", "static")
	mesh:setVertexMap(tris)
	mesh:setTexture(image)
	
	return {mesh = mesh, bw = bw, bh = h, draw = draw3x1,
		_x = x0, _y = y0, _w = w, _h = h}
end

local shader1x3 = love.graphics.newShader(pixelcode,
[[
	uniform float t[4]; // x, y, h, bh
	vec4 position(mat4 transform_projection, vec4 pos)
	{
		pos.x += t[0];
		pos.y = t[1] + sign(pos.y) * t[3] + step(0.5, pos.y) * (t[2] - 2 * t[3]) + step(1, pos.y) * t[3];
		return transform_projection * pos;
	}
]]
)

local draw1x3 = function(self, x, y, w, h, l)
	love.graphics.setShader(shader1x3)
	shader1x3:send("t", x + (w - self.bw) / 2, y, h, self.bh)
	shader1x3:send("l", l or 0)
	love.graphics.draw(self.mesh)
	love.graphics.setShader()
end

local create1x3 = function(image, x0, y0, w, h, bh)
	local W, H = image:getDimensions()
	
	local u0, u1 = x0 / W, (x0 + w) / W
	local v0, v1, v2, v3 = y0 / H, (y0 + bh) / H, (y0 + h - bh) / H, (y0 + h) / H
	
	local vertices = {
		{0, 0.0, u0, v0}, {w, 0.0, u1, v0}, 
		{0, 0.2, u0, v1}, {w, 0.2, u1, v1}, 
		{0, 0.8, u0, v2}, {w, 0.8, u1, v2}, 
		{0, 1.0, u0, v3}, {w, 1.0, u1, v3}, 
	}
	
	local tris, n, i = {}, 0, 1
	for i = 1, 5, 2 do
		tris[n + 1], tris[n + 2], tris[n + 3] = i, i + 3, i + 1
		tris[n + 4], tris[n + 5], tris[n + 6] = i, i + 3, i + 2
		n = n + 6
	end
	
	local mesh = love.graphics.newMesh(vertices, "triangles", "static")
	mesh:setVertexMap(tris)
	mesh:setTexture(image)
	
	return {mesh = mesh, bw = w, bh = bh, draw = draw1x3,
		_x = x0, _y = y0, _w = w, _h = h}
end

local shader1x1 = love.graphics.newShader(pixelcode,
[[
	uniform float t[4];
	vec4 position(mat4 transform_projection, vec4 pos)
	{
		pos.x = t[0] + pos.x * t[2];
		pos.y = t[1] + pos.y * t[3];
		return transform_projection * pos;
	}
]]
)

local draw1x1 = function(self, x, y, w, h, l)
	love.graphics.setShader(shader1x1)
	shader1x1:send("t", x, y, w, h)
	shader1x1:send("l", l or 0)
	love.graphics.draw(self.mesh)
	love.graphics.setShader()
end

local create1x1 = function(image, x0, y0, w, h)
	local W, H = image:getDimensions()
	
	local u0, u1 = x0 / W, (x0 + w) / W
	local v0, v1 = y0 / H, (y0 + h) / H
	
	local vertices = {{0, 0, u0, v0}, {1, 0, u1, v0}, {0, 1, u0, v1}, {1, 1, u1, v1}}
	
	local mesh = love.graphics.newMesh(vertices, "triangles", "static")
	mesh:setVertexMap({1,2,4, 1,4,3})
	mesh:setTexture(image)
	
	return {mesh = mesh, bw = w, bh = h, draw = draw1x1,
		_x = x0, _y = y0, _w = w, _h = h}
end


local THEME_DRAW = {}

local THEME_COLORS = {
	text = {0, 0, 0, 1},
	back = {0.82, 0.80, 0.78, 1},
	active = {0.20, 0.55, 0.80, 1},
	activetext = {0.9, 0.9, 0.9, 1},
	
	tooltip = {0.95, 0.95, 0.85, 1},
}

local image2 = lg.newImage(moduledir.."/default.png")
image2:setFilter("nearest")

local themefont = lg.newFont("assets/univga16.ttf", 16)
--print(themefont:getBaseline(), themefont:getAscent())

local THEME_FRAME = create3x3(image2, 0,0, 15,15, 4,4)
local THEME_BUTTON = create3x3(image2, 16,0, 15,15, 4,4)
local THEME_BUTTON_ON = create3x3(image2, 32,0, 15,15, 4,4)
local THEME_INPUT = create3x3(image2, 48,0, 15,15, 4,4)
local THEME_GROUP = create3x3(image2, 64,0, 15,15, 4,4)
--local THEME_INSET = create3x3(image, 40,0, 7,7, 2,2)
--local THEME_INSET2 = create3x3(image, 48,0, 7,7, 2,2)

local THEME_LINE_H = create3x1(image2, 0,16, 15,4, 3)
local THEME_LINE_V = create1x3(image2, 16,16, 4,15, 3)

local THEME_KNOB = create1x1(image2, 32, 16, 13, 13)

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
			lgprint(e.label, e.absx + (e.posw - fw) / 2, e.absy)
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
	
	local light, isdown = 0, 0
	if e == e.gui.hoverelement then light = 0.1 end

	lg.setColor(1,1,1,1)
	if (e.value and e.parent and e.parent.value == e.value) or e == e.gui.presselement then
		THEME_BUTTON_ON:draw(posx, posy, e.posw, e.posh, 0)
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
	local light = 0
	local vertical = e.isvertical
	local gui = e.gui
	--local theme = gui.theme
	
	if e == gui.presselement then
		light = 0.15
	elseif e == gui.hoverelement then
		light = 0.05
	--else
	--	light = 0.0
	end
	
	local hs = max(13, e.handlesize)
	local rpos = (e.values.current - e.values.min) / (e.values.max - e.values.min)
	
	lg.setColor(1,1,1,1)
	
	if vertical then
		THEME_LINE_V:draw(e.absx, e.absy + 2, e.posw, e.posh - 4, 0)
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
		THEME_LINE_H:draw(e.absx + 2, e.absy, e.posw - 4, e.posh, 0)
		rpos = floor(e.absx + (e.posw - hs) * rpos)
		if hs > 13 then
			THEME_BUTTON:draw(clamp(rpos, e.absx, e.absx + e.posw - hs), e.absy, hs, e.posh, light)
		else
			THEME_KNOB:draw(clamp(rpos, e.absx, e.absx + e.posw - hs), e.absy + floor(e.posh - hs), hs, hs, light)
		end
		if e.label then
			lg.setColor(colors.text)
			lg.setFont(font)
			local labelw, labelh = font:getWidth(e.label), font:getBaseline()
			lgprint(e.label, e.absx - labelw - 2, e.absy + e.posh - labelh - e.padh)
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
