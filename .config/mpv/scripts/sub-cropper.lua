local utils = require "mp.utils"

NAME_IN = 'sub-cropper.png'
NAME_OUT = 'sub-cropper.cropped.png'

-- "Inspired" by https://github.com/ObserverOfTime/mpv-scripts/blob/master/clipshot.lua
if package.config:sub(1, 1) == '\\' then -- Windows
    WINDOWS = true
    TMP = os.getenv('TEMP')..'\\'
    SHOT = TMP..NAME_IN
    CROPPED = TMP..NAME_OUT
    CLIP = {
        'powershell', '-NoProfile', '-Command', ([[& {
            Add-Type -Assembly System.Windows.Forms;
            Add-Type -Assembly System.Drawing;
            $shot = [Drawing.Image]::FromFile(%q);
            [Windows.Forms.Clipboard]::SetImage($shot);
        }]]):format(CROPPED)
    }
else -- Unix
    WINDOWS = false
    TMP = '/tmp/'
    SHOT = TMP..NAME_IN
    CROPPED = TMP..NAME_OUT
    -- os.getenv('OSTYPE') doesn't work
    local ostype = io.popen('printf "$OSTYPE"', 'r'):read()
    if ostype:sub(1, 6) == 'darwin' then -- MacOS
        CLIP = {
            'osascript', '-e', ([[¬
                set the clipboard to ( ¬
                    read (POSIX file %q) as JPEG picture ¬
                ) ¬
            ]]):format(CROPPED)
        }
    else -- Linux/BSD
        if os.getenv('XDG_SESSION_TYPE') == 'wayland' then -- Wayland
            CLIP = {'sh', '-c', ('wl-copy < %q'):format(CROPPED)}
        else -- Xorg
            CLIP = {'xclip', '-sel', 'c', '-t', 'image/png', '-i', CROPPED}
        end
    end
end

function sub_cropper_clip()
    mp.commandv("screenshot-to-file", SHOT)
    local r = utils.subprocess({ args = {"sub-cropper", SHOT, "-a"}})

    if r.status > 0 then
        mp.command_native_async({'run', unpack(CLIP)}, function(suc, _, err)
            mp.osd_message(suc and 'Copied screenshot to clipboard' or err)
        end)
    else
        mp.osd_message("No subs found")
    end
end

function sub_cropper_file()
    local outfile = os.date("%Y-%m-%d-%T.png")
    mp.commandv("screenshot-to-file", SHOT)
    local r = utils.subprocess({ args = {"sub-cropper", SHOT, "-a"}})

    if r.status > 0 then
        os.execute("mv "..CROPPED.." "..outfile)
        mp.osd_message("Saving subtitles to "..outfile)
    else
        mp.osd_message("No subs found")
    end
end

function ms_sub_cropper_clip()
    mp.commandv("screenshot-to-file", SHOT)
    local r = utils.subprocess({ args = {"wsl", "sub-cropper", SHOT, "-a"}})

    if r.status > 0 then
        mp.command_native_async({'run', unpack(CLIP)}, function(suc, _, err)
            mp.osd_message(suc and 'Copied screenshot to clipboard' or err)
        end)
    else
        mp.osd_message("No subs found")
    end
end

function ms_sub_cropper_file()
    mp.osd_message("Windows: press Y to copy cropped subs to clipboard")
    --local outfile = os.date("%Y-%m-%d-%t.png")
    --mp.commandv("screenshot-to-file", SHOT)
	--local r = utils.subprocess({ args = {"wsl", "sh", "-c", "sub-cropper $(wslpath '"..SHOT.."') -a"}})

    --if r.status > 0 then
    --    os.execute("move "..CROPPED.." "..outfile)
    --    mp.osd_message("Saving subtitles to "..outfile)
    --else
    --    mp.osd_message("No subs found")
    --end
end

if WINDOWS then
    mp.add_key_binding('y', ms_sub_cropper_file)
    mp.add_key_binding('Y', ms_sub_cropper_clip)
else
    mp.add_key_binding('y', sub_cropper_file)
    mp.add_key_binding('Y', sub_cropper_clip)
end
