# auto-width-mode

Automatically resize Emacs windows to a target width.

## Overview

auto-width-mode is a global minor mode that automatically sets the width of
windows when they gain focus.

This is particularly useful when working with multiple windows on smaller
screens, ensuring the active window maintains a readable width (e.g., 80
columns) without manual resizing each time you switch focus.

## Features

- Automatically resizes focused window to target width (default 80 columns)
- Excludes windows by major mode or buffer name pattern
- Respects window constraints (minibuffer, dedicated windows)
- Graceful handling when resizing is impossible

## Installation

### Requirements

- Emacs 25.1+

### Manual Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/sonofjon/auto-width-mode.el.git
   ```

2. Add the directory to your Emacs `load-path`:
   ```elisp
   (add-to-list 'load-path "/path/to/auto-width-mode")
   ```

3. Load and enable the mode:
   ```elisp
   (require 'auto-width-mode)
   (auto-width-mode 1)
   ```

### Using use-package

If you use `use-package`, you can install directly from GitHub:

```elisp
(use-package auto-width-mode
  :vc (:url "https://github.com/sonofjon/auto-width-mode.el.git")
  :config
  (auto-width-mode 1))
```

## Usage

Enable the mode globally:

```elisp
(auto-width-mode 1)
```

Disable the mode:

```elisp
(auto-width-mode -1)
```

## Configuration

### Target Width

Set the target width for the selected window (default is 80 columns):

```elisp
(setq auto-width-target-width 100)
```

### Exclude Major Modes

Exclude specific major modes from automatic resizing:

```elisp
(setq auto-width-exclude-modes '(dired-mode magit-status-mode compilation-mode))
```

### Exclude Buffer Name Patterns

Exclude buffers whose names match regexp patterns:

```elisp
(setq auto-width-exclude-buffer-regexp '("^\\*Messages\\*" "^\\*scratch\\*" "^\\*.*[Hh]elp\\*"))
```

### Example Configuration

```elisp
(use-package auto-width-mode
  :vc (:url "https://github.com/sonofjon/auto-width-mode.git")
  :custom
  ;; Set target width
  (auto-width-target-width 80)
  ;; Exclude some major modes
  (auto-width-exclude-modes '(dired-mode magit-status-mode))
  ;; Exclude some buffer patterns
  (auto-width-exclude-buffer-regexp '("^\\*Messages\\*" "^\\*scratch\\*"))
  :config
  (auto-width-mode 1))
```

## How It Works

auto-width-mode hooks into `window-selection-change-functions` to detect
when you switch to a different window. When a window becomes selected, the
mode attempts to resize it to the target width.

The mode:
- Only resizes eligible windows (excludes minibuffer, dedicated windows, and
  user-specified exclusions)
- Uses `window-resize` with error handling for impossible cases
- Does not continuously maintain width — manual resizing is preserved
