import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Actions.CycleWS
import qualified XMonad.StackSet as W
import System.IO
import System.Posix.Unistd

termHelios = "urxvtc"
termAxon   = "urxvtc -cd /personal"
term       = termHelios
scrotCmd   = "'%Y-%m-%d_$wx$h.png' -e 'mv $f ~/'"

modm       = mod4Mask
execKey    = controlMask .|. modm
sysexecKey = shiftMask   .|. execKey
wsKey      = shiftMask   .|. modm

volMuteKey  = 0x1008ff12
volRaiseKey = 0x1008ff13
volLowerKey = 0x1008ff11

myManHook   = composeAll
            [ className =? "<unknown>"          --> doFloat
            , className =? "qemu-system-x86_64" --> doFloat
            , className =? "Xfce4-notifyd"      --> doIgnore
            , className =? "Popup"              --> doFloat
            , resource  =? "desktop_window"     --> doIgnore
            , isFullscreen                      --> doFullFloat
            ]

newManHook = myManHook <+> manageDocks <+> manageHook defaultConfig
newLayHook = smartBorders (avoidStruts $ layoutHook defaultConfig)

main = do
     host <- fmap nodeName getSystemID
     let term = if host == "helios" then termHelios else termAxon
     xmonad $ defaultConfig
            { modMask           = mod4Mask
            , borderWidth       = 2
            -- , handleEventHook = fullscreenEventHook
            , manageHook        = newManHook
            , layoutHook        = newLayHook
            , focusFollowsMouse = True
            , terminal          = term
            }
            `additionalKeys`
            [ ((execKey, xK_a),    spawn (term ++ " -e alsamixer"))
            , ((execKey, xK_c),    spawn (term ++ " -hold -e cal -3m"))
            , ((execKey, xK_e),    spawn "claws-mail")
            , ((execKey, xK_f),    spawn (term ++ " -e newsbeuter"))
            , ((execKey, xK_h),    spawn "thunar")
            , ((execKey, xK_j),    spawn "thunar /personal")
            , ((execKey, xK_l),    spawn "i3lock -c 000000")
            , ((execKey, xK_m),    spawn (term ++ " -e htop"))
            , ((execKey, xK_p),    spawn "pidgin")
            , ((execKey, xK_r),    spawn (term ++ " -e cmus"))
            , ((execKey, xK_s),    spawn "skype")
            , ((execKey, xK_t),    spawn term)
            , ((execKey, xK_w),    spawn "firefox")
            , ((execKey, xK_x),    spawn "xchat")
            -- multimedia keys
            , ((0, volRaiseKey),   spawn "amixer set Master 2%+ unmute")
            , ((0, volLowerKey),   spawn "amixer set Master 2%- unmute")
            , ((0, volMuteKey),    spawn "amixer set Master toggle")
            -- system stuff
            , ((sysexecKey, xK_r), spawn "sudo reboot")
            , ((sysexecKey, xK_p), spawn "sudo poweroff")
            , ((sysexecKey, xK_u), spawn (term ++ " -hold -e yaourt -Syu"))
            -- other keys
            , ((modm, xK_F2),      spawn "dmenu_run -i -p '>' -nb black -nf white")
            , ((modm, xK_Print),   spawn ("sleep 0.2; scrot -s " ++ scrotCmd))
            , ((0, xK_Print),      spawn ("scrot " ++ scrotCmd))
            -- workspaces
            , ((modm, xK_Left),    moveTo  Next HiddenNonEmptyWS)
            , ((modm, xK_Right),   moveTo  Prev HiddenNonEmptyWS)
            , ((modm, xK_Up),      moveTo  Next EmptyWS)
            , ((wsKey, xK_Up),     shiftTo Next EmptyWS)
            , ((wsKey, xK_Left),   shiftTo Next HiddenNonEmptyWS)
            , ((wsKey, xK_Right),  shiftTo Prev HiddenNonEmptyWS)
            ]
