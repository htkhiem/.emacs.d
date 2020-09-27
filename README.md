## Features
- Personal minimal Emacs setup for a modern, VSCode-like experience.
- Uses Doom theme and modeline.
- Supports C/C++ and Rust mostly.
- Javadoc/Doxygen comment generation using `doc-mode`.
- Autocompletion for most languages using `company`.
- Real-time syntax error checking.
- Ctrl + mouse scrolling changes font size.
- Little bits for convenience such as system-wide clipboard, automatic braces `() {} ""` and auto update checking for MELPA modules on startup.
- Automatically downloads and installs most of its modules by itself upon cloning (no need to go here and there to redownload each module).
- Startup time kept to a minimum.
## Instructions
0. (Optional) Install Meslo Nerd Font patched by the devs of Powerlevel10k.  
`https://github.com/romkatv/powerlevel10k#meslo-nerd-font-patched-for-powerlevel10k`
1. Back up your current Emacs config folder:  
`cd ~`  
`mv .emacs.d .emacs.d.bak`  
1. Clone this repo into your home:  
`git clone https://github.com/htkhiem/.emacs.d.git`
2. Open Emacs, wait a bit for the initial setup to finish downloading the necessary packages from MELPA and run:  
`M-x all-the-icons-install-fonts`.  
3. (Optional) Run pywal once with your current wallpaper to match Emacs' colour scheme with it. Get the awesome pywal script here:  
`https://github.com/dylanaraps/pywal`  
### Uninstalling
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

### Font size control
Pratically zooming. Works in GTK mode only (for terminal mode, change your terminal font size instead - Emacs cannot control that).  
- `C-mouse wheel up`: Increments font size
- `C-mouse wheel down`: Decrements font size

### Vietnamese input
- `C-\`: toggles between English and Vietnamese Telex mode (uses built-in `leim`, no need for, and not affected by, ibus).  
Note: Emacs defaults to English on startup.

### Automatic code formatting
- `M-x RET f-a-b`: format all buffers to their respective code standards.  
Note: See `https://github.com/lassik/emacs-format-all-the-code` for external dependencies, such as clang for formatting C/C++ code.

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

## Credits:
- The Emacs and MELPA team, of course!
- domtronn's all-the-icons.el: <https://github.com/domtronn/all-the-icons.el>
- jaypei's neotree: <https://github.com/jaypei/emacs-neotree>
- The company-mode team: <https://github.com/company-mode/company-mode>
- Sarcasm's company-irony: <https://github.com/Sarcasm/company-irony>
- The Flycheck team: <https://github.com/flycheck/flycheck>
  - Also their Rust integration: <https://github.com/flycheck/flycheck-rust>
- lassik's FABulous format-all-the-code: <https://github.com/lassik/emacs-format-all-the-code>
- DarthFennec's highlight-indent-guides: <https://github.com/DarthFennec/highlight-indent-guides>
- joaotavora's yasnippet: <https://github.com/joaotavora/yasnippet>
- The Rust dev team's rust-mode for Emacs: <https://github.com/rust-lang/rust-mode>
- kwrooijen's Cargo integration : <https://github.com/kwrooijen/cargo.el>
- cataska's QML mode: <https://github.com/cataska/qml-mode>
- hlissner's Doom themes: <https://github.com/hlissner/emacs-doom-themes>
- seagle0128's Doom modeline: <https://github.com/seagle0128/doom-modeline>
- The Emacs Dashboard team: <https://github.com/emacs-dashboard/emacs-dashboard> 



