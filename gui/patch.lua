local min, max = math.min, math.max
local floor, abs = math.floor, math.abs
local function clamp(x, a, b) return min(max(x, a), b) end

local pixelcode = 
[[
	uniform float light;
	vec4 effect(vec4 color, Image tex, vec2 texture_coords, vec2 screen_coords)
	{
		vec4 texcolor = Texel(tex, texture_coords);
		texcolor.rgb *= light;
		return texcolor * color;
	}
]]

local shaders = {}
local new_shader = function(shadercode)
	local shader = shaders[shadercode]
	if shader then return shader end
	shader = love.graphics.newShader(pixelcode, shadercode)
	shaders[shadercode] = shader
	return shader
end

local new_vertmap = function(count, stride)
	local tris, n, i = {}, 1, 2
	repeat
		local j = i + stride
		tris[n    ], tris[n + 1], tris[n + 2] = i - 1, i, j
		tris[n + 3], tris[n + 4], tris[n + 5] = i - 1, j, j - 1
		n = n + 6; i = i + 1 + (1 - min(i % stride, 1))
	until i > count - stride
	--print(table.concat(tris, ","))
	return tris
end

local new_mesh = function(vertices, vertexmap, image)
	local mesh = love.graphics.newMesh(vertices, "triangles", static)
	mesh:setVertexMap(vertexmap)
	mesh:setTexture(image)
	return mesh
end

local shadercode_3x3 =
[[
	const float l = %.1f, t = %.1f, r = %.1f, b = %.1f;
	uniform float rect[4]; // x, y, w, h
	vec4 position(mat4 transform_projection, vec4 pos)
	{
		pos.x += rect[0] + (1 - step(pos.x, l)) * (rect[2] - l - r - 4.0);
		pos.y += rect[1] + (1 - step(pos.y, t)) * (rect[3] - t - b - 4.0);
		return transform_projection * pos;
	}
]]

local draw_3x3 = function(self, x, y, w, h, light)
	local shader = self.shader
	shader:send("rect", x, y, w, h)
	shader:send("light", light or 1)
	love.graphics.setShader(shader)
	love.graphics.draw(self.mesh)
	love.graphics.setShader()
end

local tris_3x3 = new_vertmap(16, 4)

local create_3x3 = function(patch, x0, y0, w, h, l, t, r, b)
	local image = patch.image
	local W, H = image:getDimensions()
	if not r then r = l end
	if not b then b = t end
	
	local u0, u1, u2, u3 = x0 / W, (x0 + l) / W, (x0 + w - r) / W, (x0 + w) / W
	local v0, v1, v2, v3 = y0 / H, (y0 + t) / H, (y0 + h - b) / H, (y0 + h) / H
	
	local x2, x3 = l + 4, l + 4 + r
	local y2, y3 = t + 4, t + 4 + b

	return {
		mesh = new_mesh(
			{{0,  0, u0, v0}, {l,  0, u1, v0}, {x2,  0, u2, v0}, {x3,  0, u3, v0}, 
			 {0,  t, u0, v1}, {l,  t, u1, v1}, {x2,  t, u2, v1}, {x3,  t, u3, v1}, 
			 {0, y2, u0, v2}, {l, y2, u1, v2}, {x2, y2, u2, v2}, {x3, y2, u3, v2}, 
			 {0, y3, u0, v3}, {l, y3, u1, v3}, {x2, y3, u2, v3}, {x3, y3, u3, v3}}, tris_3x3, image),

		shader = new_shader(shadercode_3x3:format(l, t, r, b)),
		draw = draw_3x3
	}
end

local shadercode_3x1 =
[[
	const float l = %.1f, r = %.1f;
	uniform float rect[3]; // x, y, w
	vec4 position(mat4 transform_projection, vec4 pos)
	{
		pos.x += rect[0] + (1 - step(pos.x, l)) * (rect[2] - l - r - 4.0);
		pos.y += rect[1];
		return transform_projection * pos;
	}
]]

local draw_3x1 = function(self, x, y, w, h, light)
	local shader = self.shader
	shader:send("rect", x, y + (h - self.h) / 2, w)
	shader:send("light", light or 1)
	love.graphics.setShader(shader)
	love.graphics.draw(self.mesh)
	love.graphics.setShader()
end

local tris_3x1 = new_vertmap(8, 4)

local create_3x1 = function(patch, x0, y0, w, h, l, r)
	local image = patch.image
	local W, H = image:getDimensions()
	if not r then r = l end
	
	local u0, u1, u2, u3 = x0 / W, (x0 + l) / W, (x0 + w - r) / W, (x0 + w) / W
	local v0, v1 = y0 / H, (y0 + h) / H

	local x2, x3 = l + 4, l + 4 + r
	
	return {
		mesh = new_mesh(
			{{0, 0, u0, v0}, {l, 0, u1, v0}, {x2, 0, u2, v0}, {x3, 0, u3, v0},
			 {0, h, u0, v1}, {l, h, u1, v1}, {x2, h, u2, v1}, {x3, h, u3, v1}}, tris_3x1, image),

		shader = new_shader(shadercode_3x1:format(l, r)),
		draw = draw_3x1, h = h
	}
end

local shadercode_1x3 =
[[
	const float t = %.1f, b = %.1f;
	uniform float rect[3]; // x, y, h
	vec4 position(mat4 transform_projection, vec4 pos)
	{
		pos.x += rect[0];
		pos.y += rect[1] + (1 - step(pos.y, t)) * (rect[2] - t - b - 4.0);
		return transform_projection * pos;
	}
]]

local draw_1x3 = function(self, x, y, w, h, light)
	local shader = self.shader
	shader:send("rect", x + (w - self.w) / 2, y, h)
	shader:send("light", light or 1)
	love.graphics.setShader(shader)
	love.graphics.draw(self.mesh)
	love.graphics.setShader()
end

local tris_1x3 = new_vertmap(8, 2)

local create_1x3 = function(patch, x0, y0, w, h, t, b)
	local image = patch.image
	local W, H = image:getDimensions()
	if not b then b = t end
	
	local u0, u1 = x0 / W, (x0 + w) / W
	local v0, v1, v2, v3 = y0 / H, (y0 + t) / H, (y0 + h - b) / H, (y0 + h) / H
	
	local y2, y3 = t + 4, t + 4 + b

	return {
		mesh = new_mesh(
			{{0,  0, u0, v0}, {w,  0, u1, v0},
			 {0,  t, u0, v1}, {w,  t, u1, v1},
			 {0, y2, u0, v2}, {w, y2, u1, v2},
			 {0, y3, u0, v3}, {w, y3, u1, v3}}, tris_1x3, image),

		shader = new_shader(shadercode_1x3:format(t, b)),
		draw = draw_1x3, w = w
	}
end

local shader_1x1 = love.graphics.newShader(pixelcode,
[[
	uniform float rect[2]; // x, y
	vec4 position(mat4 transform_projection, vec4 pos)
	{
		pos.x += rect[0];
		pos.y += rect[1];
		return transform_projection * pos;
	}
]])

local draw_1x1 = function(self, x, y, w, h, light)
	local shader = self.shader
	shader:send("rect", x + (w - self.w) / 2,  y + (h - self.h) / 2)
	shader:send("light", light or 1)
	love.graphics.setShader(shader)
	love.graphics.draw(self.mesh)
	love.graphics.setShader()
end

local create_1x1 = function(patch, x0, y0, w, h)
	local image = patch.image
	local W, H = image:getDimensions()
	
	local u0, u1 = x0 / W, (x0 + w) / W
	local v0, v1 = y0 / H, (y0 + h) / H
	
	return {
		mesh = new_mesh({{0, 0, u0, v0}, {w, 0, u1, v0}, {0, h, u0, v1}, {w, h, u1, v1}},
			{1,2,4, 1,4,3}, image),
	
		shader = shader_1x1, draw = draw_1x1, w = w, h = h
	}
end


local new_patch = function(image)
	return {
		image = image,
		new_3x3 = create_3x3, new_3x1 = create_3x1, new_1x3 = create_1x3, new_1x1 = create_1x1
	}
end

return {new = new_patch}