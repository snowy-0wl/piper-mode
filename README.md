# piper-mode

Text-to-speech for Emacs using the [Piper TTS](https://github.com/rhasspy/piper) engine. Provides high-quality, natural-sounding speech synthesis.

## System Requirements

- macOS on Apple Silicon (M1/M2)
- Homebrew (for dependencies)
- Emacs 27.1 or later
- `json` package 1.4 or later

## Features

- High-quality text-to-speech using Piper's neural TTS engine
- Multiple voice commands:
  - `M-x piper-speak` - Speak entered text
  - `M-x piper-speak-region` - Speak the selected text
  - `M-x piper-speak-buffer` - Speak the entire buffer
  - `M-x piper-speak-paragraph` - Speak the current paragraph
  - `M-x piper-speak-line` - Speak the current line
  - `M-x piper-speak-word` - Speak the current word
  - `M-x piper-speak-to-end` - Speak from point to end of buffer
  - `M-x piper-stop` - Stop speaking and clean up
  - `M-x piper-select-model` - Select and download a different voice model
- Automatic setup and installation of dependencies
- Support for custom voice models
- Debug logging for troubleshooting
- Temporary file management for audio output
- Process management for TTS and audio playback

## Installation

### Using straight.el (Recommended)

Add this to your Emacs configuration:

```elisp
(use-package piper-mode
  :straight (piper-mode
            :type git
            :host github
            :repo "snowy-0wl/piper-mode"
            :files ("*.el" "bin" "models" "setup-piper.sh"))
  :config
  (when (not (file-exists-p (expand-file-name "models/en_US-joe-medium.onnx"
                                             (file-name-directory (locate-library "piper-mode")))))
    (let ((default-directory (file-name-directory (locate-library "piper-mode"))))
      (shell-command "chmod +x setup-piper.sh")
      (shell-command "./setup-piper.sh .")))
  (piper-mode))
```

### Manual Installation

1. Clone the repository:
```bash
git clone https://github.com/snowy-0wl/piper-mode.git
cd piper-mode
```

2. Run the setup script:
```bash
chmod +x setup-piper.sh
./setup-piper.sh .
```

3. Add to your Emacs configuration:
```elisp
(add-to-list 'load-path "/path/to/piper-mode")
(require 'piper-mode)
(piper-mode)
```

## Configuration

The package provides several customization options:

```elisp
;; Enable debug logging
(setq piper-debug t)

;; Set custom installation directory
(setq piper-install-dir "/path/to/custom/install")

;; Set default voice model (English - Joe Medium)
(setq piper-voice-model (expand-file-name "models/en_US-joe-medium.onnx"
                                        (file-name-directory (locate-library "piper-mode"))))

;; Or use a different language model (e.g., Russian - Dmitri Medium)
(setq piper-voice-model (expand-file-name "models/ru_RU-dmitri-medium.onnx"
                                        (file-name-directory (locate-library "piper-mode"))))

;; If using use-package, you can set it in :custom section
(use-package piper-mode
  :straight (piper-mode
            :type git
            :host github
            :repo "snowy-0wl/piper-mode"
            :files ("*.el" "bin" "models" "setup-piper.sh"))
  :custom
  (piper-voice-model (expand-file-name "models/en_US-joe-medium.onnx"
                                     (file-name-directory (locate-library "piper-mode"))))
  :config
  (piper-mode))

;; Change the URL for fetching available models (if needed)
(setq piper-models-url "https://raw.githubusercontent.com/rhasspy/piper/master/VOICES.md")

;; Adjust process timeout (default: 30 seconds)
(setq piper-process-timeout 60)
```

### Voice Model Selection

Piper-mode supports multiple voice models in different languages. To change the voice model:

1. Use `M-x piper-select-model` to open the model selection interface
2. Type to filter available models (e.g., "russian" or "dmitri")
3. Select the desired model
4. The model will be automatically downloaded if not present

The default model (en_US-joe-medium) will be used until you select a different one. Your model selection persists across Emacs sessions.

## First-Time Setup

The setup script will automatically:
1. Install required dependencies:
   - onnxruntime via Homebrew (for neural network inference)
   - sox via Homebrew (for audio playback)
2. Build a forked version of espeak-ng from source (for text processing)
3. Build Piper from source
4. Download the default voice model (en_US-joe-medium, ~63MB)

This process may take a few minutes. Progress is logged to `setup.log` in the installation directory.

### Directory Structure

After installation, the following directory structure is created:
- `bin/` - Contains executables and libraries
  - `piper` - Main Piper executable
  - `espeak-ng-data/` - Speech data files
  - Various `.dylib` files for Piper, espeak-ng, and ONNX runtime
- `models/` - Voice model files
  - `en_US-joe-medium.onnx` - Default voice model
  - `en_US-joe-medium.onnx.json` - Model configuration
- `tmp/` - Temporary WAV files (automatically cleaned up)

### Troubleshooting Setup

If setup fails:

1. Check the setup log:
```bash
cat ~/.emacs.d/straight/build/piper-mode/setup.log  # If using straight.el
# or
cat /path/to/piper-mode/setup.log                   # If installed manually
```

2. Make sure Homebrew is installed and up to date:
```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
brew update
```

3. Install dependencies manually:
```bash
arch -arm64 brew install onnxruntime sox
```

4. Check file permissions:
```bash
chmod +x setup-piper.sh
chmod +x bin/piper
```

5. Verify library paths:
```bash
ls -l bin/*.dylib
```

6. Check if voice model exists:
```bash
ls -l models/en_US-joe-medium.onnx
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request. 

## License

This project is licensed under the GPL-3.0 License - see the LICENSE file for details.