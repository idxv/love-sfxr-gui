local path = (...):gsub('%.init$', '')
return require(path .. ".vgui")