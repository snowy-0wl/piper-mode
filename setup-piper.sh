#!/bin/bash

set -e
echo "Starting Piper setup..."

# Get the target installation directory from the first argument, or use script directory
if [ -n "$1" ]; then
    INSTALL_DIR="$(cd "$1" && pwd)"
else
    SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    INSTALL_DIR="$SCRIPT_DIR"
fi

mkdir -p "$INSTALL_DIR"
cd "$INSTALL_DIR"

# Create necessary directories
BIN_DIR="$INSTALL_DIR/bin"
MODELS_DIR="$INSTALL_DIR/models"
TMP_DIR="$INSTALL_DIR/tmp"
TEMP_DIR="/tmp/piper-setup"
LOG_FILE="$INSTALL_DIR/setup.log"

# Clean up existing directories if they exist
rm -rf "$BIN_DIR" "$MODELS_DIR" "$TMP_DIR" "$TEMP_DIR" espeak-ng
mkdir -p "$BIN_DIR" "$MODELS_DIR" "$TMP_DIR" "$TEMP_DIR"

# Log setup information
echo "=== Setup started at $(date) ===" | tee -a "$LOG_FILE"
echo "INSTALL_DIR=$INSTALL_DIR" | tee -a "$LOG_FILE"
echo "BIN_DIR=$BIN_DIR" | tee -a "$LOG_FILE"
echo "MODELS_DIR=$MODELS_DIR" | tee -a "$LOG_FILE"
echo "TMP_DIR=$TMP_DIR" | tee -a "$LOG_FILE"
echo "TEMP_DIR=$TEMP_DIR" | tee -a "$LOG_FILE"

echo "Setting up Piper TTS for piper-mode..." | tee -a "$LOG_FILE"

cd "$TEMP_DIR"

# Install onnxruntime
echo "Installing dependencies..." | tee -a "$LOG_FILE"
if ! brew list onnxruntime &>/dev/null; then
    arch -arm64 brew install onnxruntime
else
    echo "onnxruntime already installed" | tee -a "$LOG_FILE"
fi

# Build forked espeak-ng
echo "Building forked espeak-ng..." | tee -a "$LOG_FILE"
if [ ! -d "$BIN_DIR/espeak-ng-data" ]; then
    # Clean up any existing espeak-ng directory
    rm -rf espeak-ng
    
    # Clone and build espeak-ng
    if ! git clone https://github.com/rhasspy/espeak-ng.git; then
        echo "Failed to clone espeak-ng" | tee -a "$LOG_FILE"
        exit 1
    fi
    
    cd espeak-ng
    
    # Build and install
    if ! ./autogen.sh; then
        echo "Failed to run autogen.sh" | tee -a "$LOG_FILE"
        exit 1
    fi
    
    # Install to bin directory
    if ! ./configure --prefix="$BIN_DIR"; then
        echo "Failed to configure espeak-ng" | tee -a "$LOG_FILE"
        exit 1
    fi
    
    if ! make -j4; then
        echo "Failed to build espeak-ng" | tee -a "$LOG_FILE"
        exit 1
    fi
    
    if ! make install; then
        echo "Failed to install espeak-ng" | tee -a "$LOG_FILE"
        exit 1
    fi
    
    # Fix espeak-ng library paths
    if [ -f "$BIN_DIR/lib/libespeak-ng.1.dylib" ]; then
        # Copy to bin directory for easier access
        cp "$BIN_DIR/lib/libespeak-ng.1.dylib" "$BIN_DIR/"
        chmod +x "$BIN_DIR/libespeak-ng.1.dylib"
        
        # Fix the install name
        install_name_tool -id "@rpath/libespeak-ng.1.dylib" "$BIN_DIR/libespeak-ng.1.dylib"
    else
        echo "espeak-ng library not found" | tee -a "$LOG_FILE"
        exit 1
    fi
    
    cd "$TEMP_DIR"
fi

# Clone and build Piper
echo "Building Piper from source..." | tee -a "$LOG_FILE"
if [ ! -f "$BIN_DIR/piper" ]; then
    rm -rf piper  # Clean up any existing failed build
    git clone https://github.com/rhasspy/piper.git
    cd piper
    
    # Configure with correct espeak-ng paths
    arch -arm64 cmake -Bbuild \
        -DCMAKE_INSTALL_PREFIX=install \
        -DCMAKE_PREFIX_PATH="$BIN_DIR" \
        -DESPEAK_INCLUDE_DIR="$BIN_DIR/include" \
        -DESPEAK_LIB_DIR="$BIN_DIR/lib"
        
    arch -arm64 cmake --build build --config Release || true
    
    if [ -f "build/piper" ]; then
        # Copy necessary files
        echo "Copying files..." | tee -a "$LOG_FILE"
        cp build/piper "$BIN_DIR/"
        chmod +x "$BIN_DIR/piper"
        
        # Copy and fix piper_phonemize library
        if [ -f "build/p/src/piper_phonemize_external-build/libpiper_phonemize.1.dylib" ]; then
            cp build/p/src/piper_phonemize_external-build/libpiper_phonemize.1.dylib "$BIN_DIR/"
            chmod +x "$BIN_DIR/libpiper_phonemize.1.dylib"
            install_name_tool -id "@rpath/libpiper_phonemize.1.dylib" "$BIN_DIR/libpiper_phonemize.1.dylib"
        fi
        
        # Fix piper binary paths
        install_name_tool -add_rpath "@executable_path" "$BIN_DIR/piper"
        install_name_tool -change "@rpath/libespeak-ng.1.dylib" "@rpath/libespeak-ng.1.dylib" "$BIN_DIR/piper"
        install_name_tool -change "@rpath/libpiper_phonemize.1.dylib" "@rpath/libpiper_phonemize.1.dylib" "$BIN_DIR/piper"
    else
        echo "Failed to build piper binary" | tee -a "$LOG_FILE"
        exit 1
    fi
    cd "$TEMP_DIR"
else
    echo "Piper already installed" | tee -a "$LOG_FILE"
fi

# Copy ARM libraries
echo "Setting up libraries..." | tee -a "$LOG_FILE"
ONNX_LIB=$(brew list onnxruntime | grep libonnxruntime | grep dylib | head -n 1)
if [ ! -f "$BIN_DIR/$(basename "$ONNX_LIB")" ]; then
    cp "$ONNX_LIB" "$BIN_DIR/"
    cd "$BIN_DIR"
    ln -sf "$(basename "$ONNX_LIB")" libonnxruntime.1.14.1.dylib
fi

# Copy or download voice models
echo "Setting up voice models..." | tee -a "$LOG_FILE"
cd "$MODELS_DIR"

# Check if models exist in source directory
SOURCE_MODEL="/Users/taras/repos/piper-mode/models/en_US-joe-medium.onnx"
SOURCE_CONFIG="/Users/taras/repos/piper-mode/models/en_US-joe-medium.onnx.json"

if [ -f "$SOURCE_MODEL" ] && [ -f "$SOURCE_CONFIG" ]; then
    echo "Copying models from source directory..." | tee -a "$LOG_FILE"
    cp "$SOURCE_MODEL" "$MODELS_DIR/" || { echo "Failed to copy model file" | tee -a "$LOG_FILE"; exit 1; }
    cp "$SOURCE_CONFIG" "$MODELS_DIR/" || { echo "Failed to copy config file" | tee -a "$LOG_FILE"; exit 1; }
else
    echo "Downloading voice models..." | tee -a "$LOG_FILE"
    if [ ! -f "en_US-joe-medium.onnx" ] || [ "$(stat -f %z "en_US-joe-medium.onnx" 2>/dev/null || echo 0)" -lt 1000000 ]; then
        echo "Downloading Joe voice model..." | tee -a "$LOG_FILE"
        curl -L -o "en_US-joe-medium.onnx" \
            https://huggingface.co/rhasspy/piper-voices/resolve/v1.0.0/en/en_US/joe/medium/en_US-joe-medium.onnx || { echo "Failed to download model" | tee -a "$LOG_FILE"; exit 1; }
        curl -L -o "en_US-joe-medium.onnx.json" \
            https://huggingface.co/rhasspy/piper-voices/resolve/v1.0.0/en/en_US/joe/medium/en_US-joe-medium.onnx.json || { echo "Failed to download config" | tee -a "$LOG_FILE"; exit 1; }
    fi
fi

# Copy binaries and libraries
echo "Copying binaries and libraries..." | tee -a "$LOG_FILE"
cd "$BIN_DIR"

# Remove any existing symlinks or files
for file in /Users/taras/repos/piper-mode/bin/*; do
    if [ -f "$file" ]; then
        target="$(basename "$file")"
        rm -f "$target"
        echo "Copying $target..." | tee -a "$LOG_FILE"
        cp "$file" . || { echo "Failed to copy $target" | tee -a "$LOG_FILE"; exit 1; }
        chmod 755 "$target" || { echo "Failed to set permissions for $target" | tee -a "$LOG_FILE"; exit 1; }
    fi
done

# Handle espeak-ng data directory separately
if [ -d "/Users/taras/repos/piper-mode/bin/espeak-ng-data" ]; then
    echo "Copying espeak-ng-data..." | tee -a "$LOG_FILE"
    rm -rf espeak-ng-data
    cp -R "/Users/taras/repos/piper-mode/bin/espeak-ng-data" . || { echo "Failed to copy espeak-ng-data" | tee -a "$LOG_FILE"; exit 1; }
    chmod -R 755 espeak-ng-data || { echo "Failed to set permissions for espeak-ng-data" | tee -a "$LOG_FILE"; exit 1; }
fi

# Verify model exists
if [ ! -f "$MODELS_DIR/en_US-joe-medium.onnx" ]; then
    echo "Failed to setup voice model" | tee -a "$LOG_FILE"
    ls -l "$MODELS_DIR" | tee -a "$LOG_FILE"
    exit 1
fi

echo "Setup complete!" | tee -a "$LOG_FILE"
echo "Model directory contents:" | tee -a "$LOG_FILE"
ls -l "$MODELS_DIR" | tee -a "$LOG_FILE"
echo "Binary directory contents:" | tee -a "$LOG_FILE"
ls -l "$BIN_DIR" | tee -a "$LOG_FILE"

# Create run script
echo "Creating run script..." | tee -a "$LOG_FILE"
cat > "$BIN_DIR/run-piper.sh" << 'EOL'
#!/bin/bash

# Get the directory of this script, resolving symlinks
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do
    DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"
    SOURCE="$(readlink "$SOURCE")"
    [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE"
done
SCRIPT_DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"

# Get the models directory
MODELS_DIR="$(dirname "$SCRIPT_DIR")/models"

# Set library paths for local libraries
export PATH="$SCRIPT_DIR:$PATH"
export DYLD_LIBRARY_PATH="$SCRIPT_DIR"
export DYLD_FALLBACK_LIBRARY_PATH="$SCRIPT_DIR"

# Use local espeak-ng data
export ESPEAK_DATA_PATH="$SCRIPT_DIR/espeak-ng-data"

# Run piper with all arguments passed to this script
if [ -n "$PIPER_NO_BLOCK" ]; then
    "$SCRIPT_DIR/piper" "$@" &
else
    "$SCRIPT_DIR/piper" "$@"
fi
EOL

# Make run script executable
chmod +x "$BIN_DIR/run-piper.sh"

# Clean up
rm -rf "$TEMP_DIR"
