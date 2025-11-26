# piper-mode

Text-to-speech for Emacs using the [Piper TTS](https://github.com/rhasspy/piper) engine. Provides high-quality, natural-sounding speech synthesis.

## Features

- **High-Quality Voices**: Uses Piper's neural TTS engine for natural speech.
- **Dynamic Model Selection**: Browses and downloads voices directly from the official repository with proper language names and quality indicators.
- **Automated Setup**: Automatically downloads pre-built binaries for Linux and Intel macOS.
- **Apple Silicon Support**: Automatically compiles from source on M1/M2 Macs (official binaries are incompatible).
- **Persistent Storage**: Binaries and models are stored in `~/.emacs.d/piper-tts/` to survive package rebuilds.
- **Multiple Commands**:
  - `M-x piper-speak` - Speak entered text
  - `M-x piper-speak-region` - Speak the selected text
  - `M-x piper-speak-buffer` - Speak the entire buffer
  - `M-x piper-speak-paragraph` - Speak the current paragraph
  - `M-x piper-speak-line` - Speak the current line
  - `M-x piper-speak-word` - Speak the current word
  - `M-x piper-speak-to-end` - Speak from point to end of buffer
  - `M-x piper-stop` - Stop speaking
  - `M-x piper-pause` - Pause playback
  - `M-x piper-resume` - Resume playback
  - `M-x piper-toggle-pause` - Toggle pause/resume
  - `M-x piper-select-model` - Select and download a different voice model
  - `M-x piper-describe-model` - View detailed information about current or any voice model
  - `M-x piper-update` - Force update of binaries (rebuilds on Apple Silicon, re-downloads on Intel/Linux)
- **Playback Features**:
  - Pause and resume support
  - Text highlighting synchronized with playback
  - Gapless playback using batch concatenation (requires sox)
  - Smart text chunking at sentence boundaries

## System Requirements

- macOS (Apple Silicon or Intel) or Linux (x86_64, ARM64, ARMv7)
- `curl` and `tar` (usually pre-installed)
- `sox` (for gapless playback - install with `brew install sox` on macOS)
- Emacs 27.1 or later

## Installation

### Using straight.el (Recommended)

Add this to your Emacs configuration:

```elisp
(use-package piper-mode
  :straight (piper-mode
            :type git
            :host github
            :repo "snowy-0wl/piper-mode"
            ;; We only need the Elisp files and setup script.
            ;; Binaries/models are downloaded to ~/.emacs.d/piper-tts/ automatically.
            :files ("*.el" "setup-piper.sh"))
  :custom
  ;; Set your preferred voice model (will be auto-downloaded on first use)
  (piper-voice-model "en_US-joe-medium.onnx")
  :config
  (piper-mode))
```

On first use, it will automatically:
1. Download/build the Piper TTS engine
2. Download your chosen voice model (`en_US-joe-medium` in the example above)
3. Be ready to speak

### Manual Installation

1. Clone the repository:
```bash
git clone https://github.com/snowy-0wl/piper-mode.git
```

2. Add to your Emacs configuration:
```elisp
(add-to-list 'load-path "/path/to/piper-mode")
(require 'piper-mode)
(piper-mode)
```

## Configuration

The package works out of the box, but you can customize it:

```elisp
;; Enable debug logging
(setq piper-debug t)

;; Adjust process timeout (default: 30 seconds)
(setq piper-process-timeout 60)

;; Custom installation directory (default: ~/.emacs.d/piper-tts)
;; (setq piper-install-dir "/path/to/custom/install")

;; Voice model (required)
;; Set this to your preferred voice. Common options:
;;   "en_US-joe-medium.onnx"      - English, male (Joe)
;;   "en_US-lessac-medium.onnx"   - English, female
;;   "en_GB-alan-medium.onnx"     - British English, male
;; The model file will be auto-downloaded to ~/.emacs.d/piper-tts/models/
(setq piper-voice-model "en_US-joe-medium.onnx")

;; Customize highlight face (optional)
;; (set-face-attribute 'piper-highlight-face nil :background "#5F5F5F")
```

### Choosing Voice Models

You must configure a voice model in your init.el. The example above uses `en_US-joe-medium.onnx` (English, male).

To browse and choose a different voice:

1. `M-x piper-select-model`
2. Choose a language
3. Choose a voice and quality level
4. The model will be automatically downloaded if not already installed (shown with `[Installed]` indicator)

## How It Works

- **First Run**: When you first try to speak, `piper-mode` checks for binaries in `~/.emacs.d/piper-tts/`.
- **Automatic Setup**:
  - **Linux / Intel Mac**: Downloads official pre-built binaries (~10MB).
  - **Apple Silicon (M1/M2)**: Automatically compiles `piper` and dependencies from source. **This takes 5-10 minutes** on the first run and requires Homebrew.
- **Model Configuration**: You must configure a voice model in your init.el as shown in the installation example above.
- **Updates**: Run `M-x piper-update` to force a re-download or re-build of binaries.

## Troubleshooting

**"Piper binary not found"**
Run `M-x piper-update` to force a fresh download.

**"Model file not found"**
Ensure `piper-voice-model` is either `nil` (to use default) or points to a valid `.onnx` file. Run `M-x piper-select-model` to pick a valid one.

**Logs**
Check the `*Messages*` buffer. If `(setq piper-debug t)` is enabled, you will see detailed logs prefixed with `piper-debug:`.

## License

GPL-3.0