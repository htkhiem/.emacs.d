## Features
- Personal minimal Emacs setup for a modern, VSCode-like experience.
- Supports C/C++ and Rust mostly.
- Javadoc/Doxygen comment generation using `doc-mode`.
- Autocompletion for most languages using `company`.
- Real-time syntax error checking.
- Little bits for convenience such as system-wide clipboard, automatic braces `() {} ""` and auto update checking for MELPA modules on startup.
- Automatically downloads and installs most of its modules by itself upon cloning (no need to go here and there to redownload each module).
- Startup time kept to a minimum.
## Instructions
1. Back up your current Emacs config folder:  
`cd ~`  
`mv .emacs.d .emacs.d.bak`  
1. Clone this repo into your home:  
`git clone git@github.com:htkhiem/.emacs.d.git`
2. Open Emacs, wait a bit for the initial setup to finish downloading the necessary packages from MELPA and run:  
`M-x all-the-icons-install-fonts`.  
To restore your old Emacs config:  
`cd ~`  
`rm -rf .emacs.d`  
`mv .emacs.d.bak .emacs.d`  
## Keybinds
### Default Emacs keybinds
- `C-p`: previous line
- `C-n`: next line
- `C-a`: beginning of line
- `C-e`: end of line
- `C-f`: move forward one character
- `M-f`: move forward one word
- `C-b`: move backward one character
- `M-b`: move backward one word
- `C-spc`: start/stop highlighting
- `C-w`: cut
- `M-w`: copy
- `M-y`: paste
- `C-l`: centre view on current line, and then some
- `C-s`: search
- `M-x query-replace RET (search phrase) RET (replacement)`: search-and-replace

### Vietnamese input
- `C-\`: toggles between English and Vietnamese Telex mode (uses built-in `leim`, no need for, and not affected by, ibus).

Note: Emacs defaults to English on startup.

### Automatic code formatting
- `M-x RET f-a-b`: format all buffers to their respective code standards.

Note: See https://github.com/lassik/emacs-format-all-the-code for external dependencies, such as clang for formatting C/C++ code.

### File/window operations
- `<F8>`: open neotree sidepane (navigate using the same keys as above, enter/space to open folders or files)
- `C-x C-c`: close Emacs
- `C-x C-s`: save

### Split-screen
- `C-x 2`: split current view in 2 (top-bottom)
- `C-x 3`: same, but side-by-side
- `C-x 1`: collapse all other views, leaving just the current one
- `C-x 0`: close current one
- `Shift + arrow keys`: move between views

### doc-mode
- `C-c c d`: add/replace documentation comment at cursor.
- `C-c c r`: remove documentation comment at cursor.
- `C-c c t`: fold/unfold documentation comment at cursor.
- `C-c c r`: fold/unfold all documentation comments.

### Flycheck (real-time syntax error checking not unlike Intellisense)
- `C-c ! n`: goto next error
- `C-c ! p`: goto previous error
Explanations for errors are always shown below the mode line.
