## Features
- Personal minimal Emacs setup for a modern, VSCode-like experience.
- Supports C/C++ mostly.
- Autocompletion for most languages using `company`.
- Real-time syntax error checking.
- Little bits for convenience such as system-wide clipboard, automatic braces (} {} "" '' and auto update checking for MELPA modules on startup.
- Automatically downloads and installs most of its modules by itself upon cloning (no need to go here and there to redownload each module).
-Startup time kept to a minimum.
## Instructions
1. Install doxymacs
On Arch-based distros, do 
`yay -S doxymacs-git`. Other distros: please follow instructions at http://doxymacs.sourceforge.net/
2. Clone this repo into your home folder.
3. Open emacs and run `M-x all-the-icons-install-fonts`.

## Keybinds
### Default Emacs keybinds
- C-p: previous line
- C-n: next line
- C-a: beginning of line
- C-e: end of line
- C-f: move forward one character
- M-f: move forward one word
- C-b: move backward one character
- M-b: move backward one word
- C-spc: start/stop highlighting
- C-w: cut
- M-w: copy
- M-y: paste
- C-l: centre view on current line, and then some
- C-s: search
- M-x (enter) query-replace (enter): search-and-replace

### File/window operations
- F8: open neotree sidepane (navigate using the same keys as above, enter/space to open folders or files)
- C-x C-c: close Emacs
- C-x C-s: save

### Split-screen
- C-x 2: split current view in 2 (top-bottom)
- C-x 3: same, but side-by-side
- C-x 1: collapse all other views, leaving just the current one
- C-x 0: close current one
- Shift + arrow keys: move between views

### Doxymacs
- C-c d f: insert Doxygen comment template for the function directly below cursor
- C-c d i: insert Doxygen comment template for this file
- C-c d @: insert Doxygen comment that wraps around current selection
- C-c d ?: look up Doxygen documentation for the current symbol

### Flycheck (real-time syntax error checking not unlike Intellisense)
- C-c ! n: goto next error
- C-c ! p: goto previous error
Explanations for errors are always shown below the mode line.
