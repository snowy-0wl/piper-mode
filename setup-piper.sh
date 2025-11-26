#!/bin/bash

set -e

# Enable debug mode if requested
[ -n "$DEBUG" ] && set -x

# Error handling
set -o pipefail

# Cleanup handler
cleanup() {
    local exit_code=$?
    [ -n "$LOG_FILE" ] && log_message "INFO" "Script exiting with code: $exit_code"
    exit $exit_code
}
trap cleanup EXIT

# Logging
log_message() {
    local level="$1"
    local message="$2"
    local timestamp=$(date +"%Y-%m-%d %H:%M:%S")
    echo "[$timestamp] [$level] $message" | tee -a "$LOG_FILE"
}

# Parse arguments
UPDATE_MODE=false
while [[ "$#" -gt 0 ]]; do
    case $1 in
        --update) UPDATE_MODE=true ;;
        *) 
            if [ -z "$INSTALL_DIR_ARG" ]; then
                INSTALL_DIR_ARG="$1"
            fi
            ;;
    esac
    shift
done

# Directory setup
if [ -n "$INSTALL_DIR_ARG" ]; then
    INSTALL_DIR="$(cd "$INSTALL_DIR_ARG" && pwd)"
else
    SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    INSTALL_DIR="$SCRIPT_DIR"
fi

BIN_DIR="$INSTALL_DIR/bin"
MODELS_DIR="$INSTALL_DIR/models"
LOG_FILE="$INSTALL_DIR/setup.log"

mkdir -p "$INSTALL_DIR" "$BIN_DIR" "$MODELS_DIR"

echo "=== Setup started at $(date) ===" > "$LOG_FILE"
log_message "INFO" "Install Dir: $INSTALL_DIR"

# Check if we need to install/update
if [ -f "$BIN_DIR/piper" ] && [ "$UPDATE_MODE" = "false" ]; then
    log_message "INFO" "Piper binary exists and update not requested. Skipping installation."
    exit 0
fi

# Detect OS and Arch
OS="$(uname -s)"
ARCH="$(uname -m)"

# On macOS, check if we are on Apple Silicon even if running under Rosetta
if [ "$OS" = "Darwin" ]; then
    SYSCTL_CHECK="$(sysctl -n hw.optional.arm64 2>/dev/null)"
    log_message "INFO" "sysctl hw.optional.arm64: $SYSCTL_CHECK"
    if [ "$SYSCTL_CHECK" = "1" ]; then
        ARCH="arm64"
        log_message "INFO" "Forcing ARCH to arm64 based on sysctl"
    fi
fi

log_message "INFO" "Detected OS: $OS, Arch: $ARCH"

PIPER_VERSION="2023.11.14-2"
BASE_URL="https://github.com/rhasspy/piper/releases/download/${PIPER_VERSION}"
ASSET_NAME=""

if [ "$OS" = "Darwin" ]; then
    if [ "$ARCH" = "arm64" ]; then
        ASSET_NAME="piper_macos_aarch64.tar.gz"
    else
        ASSET_NAME="piper_macos_x64.tar.gz"
    fi
elif [ "$OS" = "Linux" ]; then
    if [ "$ARCH" = "aarch64" ] || [ "$ARCH" = "arm64" ]; then
        ASSET_NAME="piper_linux_aarch64.tar.gz"
    elif [ "$ARCH" = "x86_64" ]; then
        ASSET_NAME="piper_linux_x86_64.tar.gz"
    elif [ "$ARCH" = "armv7l" ]; then
        ASSET_NAME="piper_linux_armv7l.tar.gz"
    fi
fi

if [ "$OS" = "Darwin" ] && [ "$ARCH" = "arm64" ]; then
    log_message "INFO" "Apple Silicon detected. Official binaries are broken (x86_64). Building from source..."
    
    # Check dependencies
    if ! command -v cmake >/dev/null; then
        log_message "INFO" "Installing cmake..."
        brew install cmake
    fi
    if ! command -v brew >/dev/null; then
        log_message "ERROR" "Homebrew is required to build from source."
        exit 1
    fi
    
    # Install build deps
    log_message "INFO" "Installing build dependencies..."
    brew install cmake automake autoconf libtool pkg-config onnxruntime
    
    BUILD_DIR="$INSTALL_DIR/build"
    mkdir -p "$BUILD_DIR" "$BIN_DIR"
    cd "$BUILD_DIR"
    
    # 0. Build espeak-ng (Rhasspy fork required for specific functions)
    if [ ! -f "$INSTALL_DIR/lib/libespeak-ng.1.dylib" ] || [ "$UPDATE_MODE" = "true" ]; then
        log_message "INFO" "Building espeak-ng..."
        if [ ! -d "espeak-ng" ]; then
            git clone https://github.com/rhasspy/espeak-ng.git
            cd espeak-ng
            git checkout 2023.9.7-4
            cd ..
        fi
        cd espeak-ng
        ./autogen.sh
        ./configure --prefix="$INSTALL_DIR"
        make
        make install
        cd ..
    else
        log_message "INFO" "espeak-ng already built, skipping..."
        cd "$BUILD_DIR"
    fi
    
    # 1. Build piper-phonemize
    if [ ! -f "$INSTALL_DIR/lib/libpiper_phonemize.1.dylib" ] || [ "$UPDATE_MODE" = "true" ]; then
        log_message "INFO" "Building piper-phonemize..."
        if [ ! -d "piper-phonemize" ]; then
            git clone https://github.com/rhasspy/piper-phonemize.git
            cd piper-phonemize
            git checkout 2023.11.14-4
            cd ..
        fi
        cd piper-phonemize
        cmake -Bbuild -DCMAKE_INSTALL_PREFIX="$INSTALL_DIR" \
              -DCMAKE_BUILD_TYPE=Release \
              -DESPEAK_NG_DIR="$INSTALL_DIR"
        cmake --build build --config Release
        cmake --install build
        cd ..
    else
        log_message "INFO" "piper-phonemize already built, skipping..."
    fi
    
    # 2. Build piper
    if [ ! -f "$INSTALL_DIR/bin/piper" ] || [ "$UPDATE_MODE" = "true" ]; then
        log_message "INFO" "Building piper..."
        if [ ! -d "piper" ]; then
            git clone https://github.com/rhasspy/piper.git
            cd piper
            git checkout 2023.11.14-2
            cd ..
        fi
        cd piper
        cmake -Bbuild -DCMAKE_INSTALL_PREFIX="$INSTALL_DIR" \
              -DCMAKE_BUILD_TYPE=Release \
              -DPIPER_PHONEMIZE_DIR="$INSTALL_DIR" \
              -DONNXRUNTIME_DIR=$(brew --prefix onnxruntime)
        cmake --build build --config Release
        cmake --install build
    else
        log_message "INFO" "piper already built, skipping..."
    fi
    
    # Return to install directory
    cd "$INSTALL_DIR"
    
    # Fixup bin directory structure if needed
    # CMake install usually puts things in bin/ lib/ etc.
    # We want everything in bin/ for piper-mode simplicity or ensure paths are correct.
    # piper-mode expects bin/piper and libs in bin/
    
    # Copy piper binary
    log_message "INFO" "Copying piper binary..."
    if [ -f "$INSTALL_DIR/bin/piper" ]; then
        if [ ! "$INSTALL_DIR/bin/piper" -ef "$BIN_DIR/piper" ]; then
            cp "$INSTALL_DIR/bin/piper" "$BIN_DIR/"
            log_message "INFO" "Copied piper from install directory"
        else
            log_message "INFO" "piper binary already in bin directory"
        fi
        chmod +x "$BIN_DIR/piper"
    elif [ -f "$BUILD_DIR/piper/build/piper" ]; then
        cp "$BUILD_DIR/piper/build/piper" "$BIN_DIR/"
        chmod +x "$BIN_DIR/piper"
        log_message "INFO" "Copied piper from build directory"
    else
        log_message "ERROR" "piper binary not found!"
        exit 1
    fi
    
    # Copy libraries
    cp "$INSTALL_DIR/lib/"*.dylib "$BIN_DIR/" 2>/dev/null || true
    # Copy espeak-ng data
    # Since we built it, it should be in $INSTALL_DIR/share/espeak-ng-data
    if [ -d "$INSTALL_DIR/share/espeak-ng-data" ]; then
         rm -rf "$BIN_DIR/espeak-ng-data"
         cp -R "$INSTALL_DIR/share/espeak-ng-data" "$BIN_DIR/"
    fi
    
    # Fix library paths using install_name_tool
    log_message "INFO" "Fixing library paths..."
    chmod +x "$BIN_DIR/"*.dylib
    
    # Fix IDs of shared libraries
    if [ -f "$BIN_DIR/libespeak-ng.1.dylib" ]; then
        install_name_tool -id "@rpath/libespeak-ng.1.dylib" "$BIN_DIR/libespeak-ng.1.dylib"
    fi
    if [ -f "$BIN_DIR/libpiper_phonemize.1.dylib" ]; then
        install_name_tool -id "@rpath/libpiper_phonemize.1.dylib" "$BIN_DIR/libpiper_phonemize.1.dylib"
    fi
    
    # Fix piper binary rpath
    if [ -f "$BIN_DIR/piper" ]; then
        install_name_tool -add_rpath "@executable_path" "$BIN_DIR/piper" 2>/dev/null || true
        # Ensure it looks for libs in rpath
        install_name_tool -change "$INSTALL_DIR/lib/libespeak-ng.1.dylib" "@rpath/libespeak-ng.1.dylib" "$BIN_DIR/piper" 2>/dev/null || true
        install_name_tool -change "$INSTALL_DIR/lib/libpiper_phonemize.1.dylib" "@rpath/libpiper_phonemize.1.dylib" "$BIN_DIR/piper" 2>/dev/null || true
    fi
    
    log_message "INFO" "Build complete."
    
    # Skip the download part
    DOWNLOAD_URL=""
else
    # Standard download logic for other platforms
    if [ -z "$ASSET_NAME" ]; then
        log_message "ERROR" "Unsupported platform: $OS $ARCH"
        exit 1
    fi
    DOWNLOAD_URL="${BASE_URL}/${ASSET_NAME}"
    TEMP_ARCHIVE="$INSTALL_DIR/piper_download.tar.gz"
    log_message "INFO" "Downloading Piper from $DOWNLOAD_URL"
    if curl -L -o "$TEMP_ARCHIVE" "$DOWNLOAD_URL"; then
        log_message "INFO" "Download successful."
    else
        log_message "ERROR" "Download failed."
        exit 1
    fi
fi

if [ -n "$DOWNLOAD_URL" ]; then
    # Extract logic (only if we downloaded)
    log_message "INFO" "Extracting archive..."
    TEMP_EXTRACT="$INSTALL_DIR/piper_extract_temp"
    mkdir -p "$TEMP_EXTRACT"
    tar -xzf "$TEMP_ARCHIVE" -C "$TEMP_EXTRACT"
    
    if [ -d "$TEMP_EXTRACT/piper" ]; then
        log_message "INFO" "Moving files to $BIN_DIR"
        rm -rf "$BIN_DIR"/*
        cp -R "$TEMP_EXTRACT/piper/"* "$BIN_DIR/"
        
        # Extract complete
        
        # On macOS (Intel), we need espeak-ng libraries which are missing from the release.
        if [ "$OS" = "Darwin" ]; then
            log_message "INFO" "Searching for espeak-ng libraries..."
            ESPEAK_LIB=""
            
            # Check Homebrew locations
            if [ -f "/opt/homebrew/lib/libespeak-ng.dylib" ]; then
                ESPEAK_LIB="/opt/homebrew/lib/libespeak-ng.dylib"
            elif [ -f "/usr/local/lib/libespeak-ng.dylib" ]; then
                ESPEAK_LIB="/usr/local/lib/libespeak-ng.dylib"
            fi
            
            # If not found, try to install via Homebrew
            if [ -z "$ESPEAK_LIB" ]; then
                if command -v brew >/dev/null 2>&1; then
                    log_message "INFO" "espeak-ng not found. Installing via Homebrew..."
                    brew install espeak-ng
                    
                    # Re-check locations
                    if [ -f "/opt/homebrew/lib/libespeak-ng.dylib" ]; then
                        ESPEAK_LIB="/opt/homebrew/lib/libespeak-ng.dylib"
                    elif [ -f "/usr/local/lib/libespeak-ng.dylib" ]; then
                        ESPEAK_LIB="/usr/local/lib/libespeak-ng.dylib"
                    fi
                else
                    log_message "WARNING" "Homebrew not found. Cannot auto-install espeak-ng."
                fi
            fi
            
            if [ -n "$ESPEAK_LIB" ]; then
                log_message "INFO" "Found espeak-ng at $ESPEAK_LIB. Symlinking..."
                ln -sf "$ESPEAK_LIB" "$BIN_DIR/libespeak-ng.1.dylib"
                ln -sf "$ESPEAK_LIB" "$BIN_DIR/libespeak-ng.dylib"
            else
                log_message "WARNING" "libespeak-ng.dylib not found! You may need to run: brew install espeak-ng"
            fi
            
            # Quarantine handling
            log_message "INFO" "Removing macOS quarantine attributes..."
            xattr -d com.apple.quarantine "$BIN_DIR/piper" 2>/dev/null || true
            xattr -d com.apple.quarantine "$BIN_DIR"/*.dylib 2>/dev/null || true
            if [ -d "$BIN_DIR/espeak-ng-data" ]; then
                 find "$BIN_DIR/espeak-ng-data" -exec xattr -d com.apple.quarantine {} + 2>/dev/null || true
            fi
        fi
    fi
    rm -f "$TEMP_ARCHIVE"
    rm -rf "$TEMP_EXTRACT"
fi


# Create run script wrapper
log_message "INFO" "Creating run script..."
cat > "$BIN_DIR/piper-with-phonemes" << 'EOL'
#!/bin/bash
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do
    DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
    SOURCE="$(readlink "$SOURCE")"
    [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE"
done
SCRIPT_DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"

# Set library paths
export DYLD_LIBRARY_PATH="$SCRIPT_DIR:$DYLD_LIBRARY_PATH"
export LD_LIBRARY_PATH="$SCRIPT_DIR:$LD_LIBRARY_PATH"
export ESPEAK_DATA_PATH="$SCRIPT_DIR/espeak-ng-data"

# Run piper
"$SCRIPT_DIR/piper" "$@"
EOL

chmod +x "$BIN_DIR/piper-with-phonemes"

# Verify binary architecture
if [ -f "$BIN_DIR/piper" ]; then
    ARCH_CHECK=$(file "$BIN_DIR/piper")
    log_message "INFO" "Installed binary architecture: $ARCH_CHECK"
fi

log_message "INFO" "Setup complete."
exit 0
