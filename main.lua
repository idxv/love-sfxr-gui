-- This is a modified copy that requires love version >= 11
if love._version_major < 11 then
	error("This program requires version 11 or above")
end

local gui = require('gui'):new()
--local font = love.graphics.newFont("xp-tahoma.otf", 16, "none")
local font = love.graphics.newFont("assets/univga16.ttf", 16, "none")
font:setFilter("nearest")
--local font = love.graphics.newFont(14)

local sfxr = require("sfxr")
local sound, source, wavecanvas
local seed, lockseed = 0, false

local message_queue = {}
local insert_message = function(message)
	if #message_queue >= 10 then table.remove(message_queue) end
	table.insert(message_queue, 1, message)
end

love.graphics.setBackgroundColor(0.3,0.7,0.5,1)

function updateWaveCanvas(waveview)
    local w, h = wavecanvas:getDimensions()
    love.graphics.setCanvas(wavecanvas)
    love.graphics.clear(.25, .25, .25, 1)
    love.graphics.setColor(.7, .7, .7, 1)
	local ls = love.graphics.getLineStyle()
    love.graphics.setLineStyle("rough")

    -- Iterate through the passed table and draw all lines to the canvas
    local step = w / #waveview
    local last = h/2
    for i, v in ipairs(waveview) do
        local x = (i * step)
        local y = (-v + 1) * h/2

        love.graphics.line(x - step, last, x, y)
        last = y
    end

    -- Draw the zero line
    love.graphics.setColor(1, 0.314, 0.2, 0.784)
    love.graphics.line(0, h/2, w, h/2)

    love.graphics.setCanvas()
    --t = love.timer.getTime() - t
    --statistics.waveview = math.floor(t * 10000) / 10
	love.graphics.setLineStyle(ls)
end

function playsound()
	sound._isdirty = nil
    -- Stop the currently playing source
    if source then
        source:stop()
    end

    --local t = love.timer.getTime()
    local tab = sound:generateTable(sfxr.FREQ_44100, sfxr.BITS_FLOAT)
    --t = love.timer.getTime() - t
    --statistics.generation = math.floor(t * 10000) / 10

    if #tab == 0 then
        return nil
    end

    sounddata = love.sound.newSoundData(#tab, 44100, 16, 1)
    --statistics.duration = math.floor(sounddata:getDuration() * 10000) / 10

    -- Stuff for the wave view
    local waveview = {}
    local j = 0
    local max = -1
    local min = 1
    local avg = 0.5

    --local t = love.timer.getTime()
    for i = 0, #tab - 1 do

        local v = tab[i + 1]
        -- Copy the sample over to the SoundData
        sounddata:setSample(i, v)

        -- Add the minimal and maximal sample to the wave view
        -- every 256 samples. This is how Audacity does it, actually.
        j = j + 1
        min = math.min(v, min)
        max = math.max(v, max)
        if j >= 256 then
            waveview[#waveview + 1] = min
            waveview[#waveview + 1] = max
            j = 0
            min, max = 1, -1
        end
    end
   -- t = love.timer.getTime() - t
    --statistics.transfer = math.floor(t * 10000) / 10

    updateWaveCanvas(waveview)
    --updateStatistics()

    if sounddata then
        source = love.audio.newSource(sounddata)
        source:play()
       -- playbutton:SetText("Stop Playing")
        --playing = true
    end
end

local guicategories = {
    {
        "Envelope",
        "envelope",
        {
            {"Attack Time", "envelope_attack", 0, 1},
            {"Sustain Time", "envelope_sustain", 0, 1},
            {"Sustain Punch", "envelope_punch", 0, 1},
            {"Decay Time", "envelope_decay", 0, 1}
        }
    },
    {
        "Frequency",
        "frequency",
        {
            {"Start", "frequency_start", 0, 1},
            {"Minimum", "frequency_min", 0, 1},
            {"Slide", "frequency_slide", -1, 1},
            {"Delta Slide", "frequency_dslide", -1, 1}
        }
    },
    {
        "Vibrato",
        "vibrato",
        {
            {"Depth", "vibrato_depth", 0, 1},
            {"Speed", "vibrato_speed", 0, 1}
        }
    },
    {
        "Change",
        "change",
        {
            {"Amount", "change_amount", -1, 1},
            {"Speed", "change_speed", 0, 1}
        }
    },
    {
        "Square Duty",
        "duty",
        {
            {"Ratio", "duty_ratio", 0, 1},
            {"Sweep", "duty_sweep", -1, 1}
        }
    },
    {
        "Phaser",
        "phaser",
        {
            {"Offset", "phaser_offset", -1, 1},
            {"Sweep", "phaser_sweep", -1, 1}
        }
    },
    {
        "Low Pass",
        "lowpass",
        {
            {"Cutoff", "lowpass_cutoff", 0, 1},
            {"Sweep", "lowpass_sweep", -1, 1},
            {"Resonance", "lowpass_resonance", 0, 1}
        }
    },
    {
        "High Pass",
        "highpass",
        {
            {"Cutoff", "highpass_cutoff", 0, 1},
            {"Sweep", "highpass_sweep", -1, 1}
        }
    }
}

local waveFormList = {
	["Square"] = 0,
	["Sawtooth"] = 1,
	["Sine"] = 2,
	["Noise"] = 3,
	[0] = "Square",
	[1] = "Sawtooth",
	[2] = "Sine",
	[3] = "Noise"
}

local generators = {
	{"Pickup/Coin", "randomPickup"},
	{"Laser/Shoot", "randomLaser"},
	{"Explosion", "randomExplosion"},
	{"Powerup", "randomPowerup"},
	{"Hit/Hurt", "randomHit"},
	{"Jump", "randomJump"},
	{"Blip/Select", "randomBlip"}
}

local round = function(x, decimals)
	if x >= 0 then return math.floor(x * decimals + 0.5) / decimals end
	return math.ceil(x * decimals - 0.5) / decimals
end

local soundslider_setvalue = function(this, value)
	if value then this.values.current = value
	else value = this.values.current end
	value = round(value, 100)
	this.valuetext.label = value
	this.roundvalue = value
end

local soundslider_onchange = function(this)
	this:setvalue()
	sound[this.param] = this.roundvalue
	sound._isdirty = true
end

local soundslider_updatevalue = function(this)
	this:setvalue(sound[this.param])
end

local whitecolor = {1,1,1,1}

local function newSoundSlider(label, param, pos, parent, value, minv, maxv)
	if not minv then minv = 0 end
	if not maxv then maxv = 1 end
	if not value then value = minv end
	
	local slider = gui:scroll(label, pos, parent, {minv, maxv, value, 1 / 100, "horizontal"})
	local y = (slider.posh - gui.theme.font:getHeight()) / 2
	slider.valuetext = gui:text("-8.88", {slider.posw + 4, y}, slider, true)

	slider.setvalue = soundslider_setvalue
	slider:setvalue()

	slider.onchange = soundslider_onchange
	slider.updatevalue = soundslider_updatevalue
	
	slider.keyrepeat = true
	slider.enter = soundslider_enter
	slider.leave = soundslider_leave
	
	--slider.style.unit = slider.posh
	slider.param = param
	
	slider.handlesize = 13

	return slider
end

local _pos = {}
local pos = function(x, y, w, h)
	_pos.x, _pos.y, _pos.w, _pos.h = x, y, w, h
	return _pos
end

love.load = function()
	love.graphics.setColor(1, 0, 0, 1) -- just setting these so we know the gui isn't stealing our thunder

	sound = sfxr.newSound()
	wavecanvas = love.graphics.newCanvas(300, 300)
	
	local fw = font:getWidth("m")
	local fh = font:getHeight()
	local ascent = font:getAscent()
	local pad = gui.theme.padh
	--local padw, padh = 3, 3
	
	gui.theme.font = font
	gui.theme.unit = font:getHeight() + 2 * pad
	local unit = gui.theme.unit
	
	-- PARAMETERS
	
	local x = font:getWidth("Sustain PunchM")
	local width = x + 150 + font:getWidth("M-8.88") + 2 * pad
	local height = unit
	
	local maxheight = 590
	paramsgp = gui:collapsegroup('Parameters', pos(10, 5, width, maxheight))
	paramsgp.drag = true
	
	local gp
	
	width = width - 2 * pad
		
	gp = gui:group("Waveform", pos(pad, y, width, fh + 2 * pad + unit), paramsgp)
	
	local xx = pad
	for i = 0, 3 do
		local v = waveFormList[i]
		local w = font:getWidth(v) + 4 * pad
		local b = gui:option(v, pos(xx, fh + pad, w, unit), gp, i)
		b.click = function(this)
			sound.waveform = this.value
			this.parent.value = this.value
			playsound()
		end
		
		xx = xx + w + pad
	end
	
	xx = math.floor((width - xx) / 5)
	for i = gp.numoverlay + 1, #gp.children do
		b = gp.children[i]
		b.posx = b.posx + i*xx
	end
	local waveformgp = gp
	--gp.pos.h = 2*(fh + pad) + pad
	
	newSoundSlider("Repeat Speed", "repeatspeed",
		pos(x + pad, 0, 150, 12), paramsgp, 0, -1, 1)
	
	slider = newSoundSlider("Volume", "soundvolume",
		pos(x + pad, 0, 150, 12), paramsgp, 0, 0, 1)
	
	for i, v in ipairs(guicategories) do
		gp = gui:group(v[1], pos(pad, 0, width, 0), paramsgp)
		
		for j, s in ipairs(v[3]) do
			newSoundSlider(s[1], s[2], pos(x, 0, 150, 12), gp, 0, s[3], s[4])
		end
		gp:verticaltile()
	end
	paramsgp:verticaltile()
	
	if paramsgp.posh > maxheight then
		local sbw = math.floor(unit/2) + 2
		paramsgp.posw = paramsgp.posw + sbw
		paramsgp:updateheight(maxheight)
		local scrollgp = gui:scrollgroup(nil, pos(0, paramsgp.unit,
			paramsgp.posw - sbw - math.floor(pad / 2), maxheight - paramsgp.unit - paramsgp.padh),
			paramsgp, "vertical")
		
		local children = paramsgp.children
		local start = paramsgp.numoverlay + 1
		for i = start, #children - 1 do
			v = table.remove(children, start)
			table.insert(scrollgp.children, v)
			v.parent = scrollgp
			v.posy = v.posy - scrollgp.posy
		end
		paramsgp.control.posx = paramsgp.control.posx + sbw
		scrollgp:updatecontent()
		paramsgp.modifiers = scrollgp.children
	else
		--paramsgp:updateheight(y)
		paramsgp.modifiers = paramsgp.children
	end
	
	
	paramsgp.updatevalues = function(this)
		waveformgp.value = sound.waveform
		for i, v in ipairs(this.modifiers) do
			if v.classname == "group" then
				for j, s in ipairs(v.children) do
					if s.updatevalue then s:updatevalue() end
				end
			elseif v.updatevalue then v:updatevalue()
			end
		end
	end
	
	paramsgp:updatevalues()

	-- RANDOM
	
	x = 0
	width = x + font:getWidth("MMLaser/ShootMM") + 2 * pad
	height = unit + pad
	
	local randgp = gui:collapsegroup('Random', pos(10, 10, width, height))
	randgp.drag = true
	
	width = width - 2 * pad
	
	local seedw = font:getWidth("Seed")
	local seedin = gui:input("Seed", pos(x + pad + seedw, 0, width - seedw - pad, fh + 4), randgp)
	seedin.value = tostring(seed)
	seedin.keyrepeat = true
	
	seedin.updatevalue = function(this)
		local value = tonumber(this.value)
		if not value then value = 0; this.value = "0" end
		seed = value
	end
	
	seedin.unfocus = function(this)
		this:updatevalue()
	end
	seedin.done = function(this)
		this:updatevalue()
		this.gui:unfocus()
	end

	local lockseedopt = gui:checkbox("Lock Seed", pos(x + 2 * pad, 0, 16, 16), randgp)
	width = width - 2 * pad -- button width


	gp = gui:group("Generators", pos(pad, 0, width + 2 * pad, fh + pad), randgp)

	local button
	for i, v in ipairs(generators) do
		button = gui:button(v[1], pos(pad, 0, width, unit), gp)
		button.click = function(this)
			sound[v[2]](sound, seed)
			paramsgp:updatevalues()
			playsound()
			if not lockseedopt.value then
				seed = seed + 1
				seedin.value = tostring(seed)
			end
		end
	end
	gp:verticaltile()
	

	
	
	gp = gui:group("Randomizers", pos(pad, 0, width + 2 * pad, fh + pad), randgp)
	
	button = gui:button("Mutate", pos(x + pad, 0, width, unit), gp)
	button.click = function(this)
		sound:mutate(seed)
		paramsgp:updatevalues()
		playsound()
	end
	
	button = gui:button("Randomize", pos(x + pad, 0, width, unit), gp)
	button.click = function(this)
		seed = seed + 1
		seedin.value = tostring(seed)
		sound:randomize(seed)
		paramsgp:updatevalues()
		playsound()
	end
	
	gp:verticaltile()
	
	randgp:verticaltile()
	paramsgp.posx = 800 - paramsgp.posw - 10 --randgp.posx + randgp.posw + 10
	
	
	-- WAVE VIEW
	x = pad
	width, height =  wavecanvas:getDimensions()
	
	local waveviewgp = gui:collapsegroup('Wave View', 
		pos(10, 10, width + 2 * pad, unit + height + pad))
	waveviewgp.drag = true
	gui:image(nil, pos(x, unit, width, height), waveviewgp, wavecanvas)
	waveviewgp.posx = paramsgp.posx - waveviewgp.posw - 10
	
	
	--ACTIONS
	x = 0
	width = randgp.posw
	height = unit
	
	local savegp = gui:collapsegroup('Actions', pos(10, 10, width, height))
	savegp.drag = true
	
	width = width - 2 * pad
	
	button = gui:button("Play", pos(x + pad, 0, width, unit), savegp)
	--local pimage = love.graphics.newImage("assets/generic-rpg-loot01.png")
	--button.image = pimage
	button.click = function(this)
		if sound._isdirty then playsound() return end
		
		if not source then return end
		source:stop()
		source:play()
	end
	
	gui:text("Filename", pos(x + 2 * pad, 0, width, fh), savegp, false)
	
	local filein = gui:input(nil, pos(x + 2 * pad, 0, width - 2 * pad, fh + 4), savegp, "newfile")
	filein.keyrepeat = true
	
	gp = gui:group(nil, pos(pad, 0, width, pad), savegp)
	
	width = width - 2 * pad
	
	local saveaction = function(name, ext, method, ...)
		local filename = name .. ext
		local file = love.filesystem.newFile(filename)
		local ok, message = file:open("w")
		if ok then
			sound[method](sound, file, ...)
			file:close()
			message = string.format("Saved as %s/%s", love.filesystem.getSaveDirectory(), filename)
		end
		insert_message(message)
	end
	
	button = gui:button("Save Lua", pos(x + pad, 0, width, unit), gp)
	button.click = function(this)
		saveaction(filein.value, ".lua", "save")
	end
	
	button = gui:button("Load Lua", pos(x + pad, 0, width, unit), gp)
	button.click = function(this)
		insert_message("Please buy the full version to unlock this feature")
	end
	
	button = gui:button("Save Binary", pos(x + pad, 0, width, unit), gp)
	button.click = function(this)
		saveaction(filein.value, ".sfxr", "saveBinary")
	end
	
	button = gui:button("Load Binary", pos(x + pad, 0, width, unit), gp)
	button.click = function(this)
		local filename = filein.value .. ".sfxr"
		local file = love.filesystem.newFile(filename)
		local ok, message = file:open("r")
		if ok then
			sound["loadBinary"](sound, file)
			file:close()
			message = string.format("Saved as %s/%s", love.filesystem.getSaveDirectory(), filename)
			paramsgp:updatevalues()
			playsound()
		end
		insert_message(message)
	end
	
	button = gui:button("Export Wav", pos(x + pad, 0, width, unit), gp)
	button.click = function(this)
		saveaction(filein.value, ".wav", "exportWAV")
	end
	
	gp:verticaltile()
	savegp:verticaltile()
	savegp.posx = randgp.posx
	savegp.posy = randgp.posy + randgp.posh + 10
	
	
	gui:updatepositions()
	playsound()
end

love.update = function(dt)
	gui:update(dt)
end

love.draw = function()
	love.graphics.setColor(0,0,0, 1)
	for i, v in ipairs(message_queue) do
		love.graphics.print(v, font, 5, i * 14 - 10)
	end
	gui:draw()
	--love.graphics.print(("%i, %i"):format(love.mouse.getPosition()), 0, 0)
end

love.keypressed = function(key, code, isrepeat)
	gui:keypress(key)
end

love.textinput = function(key)
	gui:textinput(key)
end

-- deal with 0.10 mouse API changes
love.mousepressed = function(x, y, button)
	gui:mousepress(x, y, button) -- pretty sure you want to register mouse events
end
love.mousereleased = function(x, y, button)
	gui:mouserelease(x, y, button)
end
love.wheelmoved = function(x, y)
	gui:mousewheel(x, y)
end
