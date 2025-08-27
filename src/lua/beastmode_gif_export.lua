function saveFrame(filename)
  local screenshot = love.graphics.captureScreenshot(filename)
  screenshot:encode("png", filename)
end
-- Inside love.draw(), at each frame:
saveFrame(string.format("temp/frame_%04d.png", frame_number))
ffmpeg -framerate 10 -i temp/frame_%04d.png -vf "scale=320:-1" -gifflags +transdiff -y output.gif
<img src="path/to/output.gif" alt="SlopBucket BeastMode" style="max-width:100%">
