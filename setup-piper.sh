#!/bin/bash

set -e

# Enable debug mode if requested
[ -n "$DEBUG" ] && set -x

# Error handling
set -o pipefail  # Ensure pipeline errors are caught

# Cleanup handler
cleanup() {
    local exit_code=$?
    [ -n "$LOG_FILE" ] && log_message "INFO" "Script exiting with code: $exit_code"
    exit $exit_code
}
trap cleanup EXIT

# Version and state management
STATE_DIR=""
STATE_FILE=""
VERSION_FILE=""
CURRENT_STATE="{}"

#######################
# Utility Functions   #
#######################

# Log with timestamp
log_message() {
    local level="$1"
    local message="$2"
    local timestamp=$(date +"%Y-%m-%d %H:%M:%S")
    echo "[$timestamp] [$level] $message" | tee -a "$LOG_FILE"
}

# Function to calculate file checksum
get_file_checksum() {
    local file="$1"
    if [ -f "$file" ]; then
        shasum -a 256 "$file" | cut -d' ' -f1
    else
        echo ""
    fi
}

# Function to manage temporary directories
manage_temp_dirs() {
    local action="$1"
    local dir="$2"
    local max_age="$3"
    
    case "$action" in
        "init")
            if [ -d "$dir" ]; then
                # Clean old files (older than max_age days)
                find "$dir" -type f -mtime +"$max_age" -delete 2>/dev/null
                find "$dir" -type d -empty -delete 2>/dev/null
            fi
            mkdir -p "$dir"
            ;;
        "cleanup")
            if [ -d "$dir" ]; then
                find "$dir" -type f -delete 2>/dev/null
                find "$dir" -type d -empty -delete 2>/dev/null
            fi
            ;;
    esac
}

#######################
# Version Management  #
#######################

# Function to get espeak version
get_espeak_version() {
    local dir="$1"
    if [ -d "$dir" ]; then
        cd "$dir"
        git rev-parse HEAD 2>/dev/null || echo ""
        cd - > /dev/null
    else
        echo ""
    fi
}

# Function to get piper version
get_piper_version() {
    local dir="$1"
    if [ -d "$dir" ]; then
        cd "$dir"
        git rev-parse HEAD 2>/dev/null || echo ""
        cd - > /dev/null
    else
        echo ""
    fi
}

#######################
# State Management   #
#######################

# Initialize state management
init_state_management() {
    local install_dir="$1"
    STATE_DIR="${install_dir}/.state"
    STATE_FILE="${STATE_DIR}/state.json"
    VERSION_FILE="${STATE_DIR}/versions.json"
    
    # Create state directory if it doesn't exist
    mkdir -p "$STATE_DIR"
    
    # Initialize state file if it doesn't exist
    if [ ! -f "$STATE_FILE" ]; then
        echo '{"last_update": null, "components": {}}' > "$STATE_FILE"
    fi
    
    # Initialize version file if it doesn't exist
    if [ ! -f "$VERSION_FILE" ]; then
        echo '{
            "espeak_ng": null,
            "piper": null,
            "model_checksums": {}
        }' > "$VERSION_FILE"
    fi
    
    # Load current state
    CURRENT_STATE=$(cat "$STATE_FILE")
}

# Save state to file
save_state() {
    local component="$1"
    local version="$2"
    local timestamp=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
    
    # Update state
    echo "$CURRENT_STATE" | jq \
        --arg comp "$component" \
        --arg ver "$version" \
        --arg time "$timestamp" \
        '.last_update = $time | .components[$comp] = $ver' > "$STATE_FILE"
    
    # Reload current state
    CURRENT_STATE=$(cat "$STATE_FILE")
}

# Get component state
get_component_state() {
    local component="$1"
    echo "$CURRENT_STATE" | jq -r ".components[\"$component\"] // \"\"" 
}

#######################
# Model Management   #
#######################

# Function to verify model checksum
verify_model_checksum() {
    local model_file="$1"
    local model_name="$2"
    
    if [ ! -f "$model_file" ]; then
        return 1
    fi
    
    local current_checksum=$(get_file_checksum "$model_file")
    local stored_checksum=$(echo "$CURRENT_STATE" | jq -r ".components.model_checksums[\"$model_name\"] // \"\"")
    
    if [ -n "$stored_checksum" ] && [ "$current_checksum" = "$stored_checksum" ]; then
        return 0
    fi
    
    return 1
}

# Function to save model checksum
save_model_checksum() {
    local model_file="$1"
    local model_name="$2"
    local checksum=$(get_file_checksum "$model_file")
    
    # Update state with new checksum
    CURRENT_STATE=$(echo "$CURRENT_STATE" | jq \
        --arg name "$model_name" \
        --arg sum "$checksum" \
        '.components.model_checksums[$name] = $sum')
    echo "$CURRENT_STATE" > "$STATE_FILE"
}

echo "Starting Piper setup..."

# Get the target installation directory from the first argument, or use script directory
if [ -n "$1" ]; then
    INSTALL_DIR="$(cd "$1" && pwd)"
else
    SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    INSTALL_DIR="$SCRIPT_DIR"
fi

# Initialize state management
init_state_management "$INSTALL_DIR"

mkdir -p "$INSTALL_DIR"
cd "$INSTALL_DIR"

# Function to manage temporary directories
manage_temp_dirs() {
    local action="$1"
    local dir="$2"
    local max_age="$3"
    
    case "$action" in
        "init")
            if [ -d "$dir" ]; then
                # Clean old files (older than max_age days)
                find "$dir" -type f -mtime +"$max_age" -delete 2>/dev/null
                find "$dir" -type d -empty -delete 2>/dev/null
            fi
            mkdir -p "$dir"
            ;;
        "cleanup")
            if [ -d "$dir" ]; then
                find "$dir" -type f -delete 2>/dev/null
                find "$dir" -type d -empty -delete 2>/dev/null
            fi
            ;;
    esac
}

# Create necessary directories
BIN_DIR="$INSTALL_DIR/bin"
MODELS_DIR="$INSTALL_DIR/models"
TMP_DIR="$INSTALL_DIR/tmp"
TEMP_DIR="/tmp/piper-setup"
BUILD_DIR="$TEMP_DIR/build"
ESPEAK_BUILD_DIR="$BUILD_DIR/espeak-ng"
PIPER_BUILD_DIR="$BUILD_DIR/piper"
LOG_FILE="$INSTALL_DIR/setup.log"

# Initialize all directories
log_message "INFO" "Initializing directories..."

# Create permanent directories
mkdir -p "$BIN_DIR" "$MODELS_DIR" "$TMP_DIR"

# Initialize temp directories with cleanup of old files
manage_temp_dirs "init" "$TEMP_DIR" 7
manage_temp_dirs "init" "$BUILD_DIR" 7
manage_temp_dirs "init" "$ESPEAK_BUILD_DIR" 7
manage_temp_dirs "init" "$PIPER_BUILD_DIR" 7

# Track directories in state
echo "$CURRENT_STATE" | jq \
    --arg tmp "$TMP_DIR" \
    --arg temp "$TEMP_DIR" \
    --arg build "$BUILD_DIR" \
    '.directories = {"tmp": $tmp, "temp": $temp, "build": $build}' > "$STATE_FILE"

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

# Function to get espeak-ng version from git
get_espeak_version() {
    local dir="$1"
    if [ -d "$dir" ]; then
        cd "$dir"
        git rev-parse HEAD 2>/dev/null || echo ""
        cd - > /dev/null
    else
        echo ""
    fi
}

# Build forked espeak-ng
log_message "INFO" "Checking espeak-ng status..."

# Get current version if installed
CURRENT_ESPEAK_VERSION=""
if [ -d "$BIN_DIR/espeak-ng-data" ]; then
    CURRENT_ESPEAK_VERSION=$(get_component_state "espeak_ng")
    log_message "INFO" "Current espeak-ng version: $CURRENT_ESPEAK_VERSION"
fi

# Get latest version from remote
REMOTE_ESPEAK_VERSION=$(git ls-remote https://github.com/rhasspy/espeak-ng.git HEAD | cut -f1)
log_message "INFO" "Remote espeak-ng version: $REMOTE_ESPEAK_VERSION"

# Check if we need to rebuild
if [ "$CURRENT_ESPEAK_VERSION" = "$REMOTE_ESPEAK_VERSION" ] && [ -d "$BIN_DIR/espeak-ng-data" ]; then
    log_message "INFO" "espeak-ng is up to date, skipping rebuild"
else
    log_message "INFO" "Building forked espeak-ng..."
    # Clean up any existing espeak-ng directory
    rm -rf espeak-ng
    
    # Clone and build espeak-ng
    if ! git clone https://github.com/rhasspy/espeak-ng.git; then
        log_message "ERROR" "Failed to clone espeak-ng"
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
    
    # Save new version to state
    save_state "espeak_ng" "$REMOTE_ESPEAK_VERSION"
    log_message "INFO" "Updated espeak-ng state to: $REMOTE_ESPEAK_VERSION"
fi

# Function to get piper version
get_piper_version() {
    local dir="$1"
    if [ -d "$dir" ]; then
        cd "$dir"
        git rev-parse HEAD 2>/dev/null || echo ""
        cd - > /dev/null
    else
        echo ""
    fi
}

# Check and build Piper
log_message "INFO" "Checking Piper status..."

# Get current version if installed
CURRENT_PIPER_VERSION=""
if [ -f "$BIN_DIR/piper" ]; then
    CURRENT_PIPER_VERSION=$(get_component_state "piper")
    log_message "INFO" "Current Piper version: $CURRENT_PIPER_VERSION"
fi

# Get latest version from remote
REMOTE_PIPER_VERSION=$(git ls-remote https://github.com/rhasspy/piper.git HEAD | cut -f1)
log_message "INFO" "Remote Piper version: $REMOTE_PIPER_VERSION"

# Check if we need to rebuild
if [ "$CURRENT_PIPER_VERSION" = "$REMOTE_PIPER_VERSION" ] && [ -f "$BIN_DIR/piper" ]; then
    log_message "INFO" "Piper is up to date, skipping rebuild"
else
    log_message "INFO" "Building Piper from source..."
    rm -rf piper  # Clean up any existing failed build
    if ! git clone https://github.com/rhasspy/piper.git; then
        log_message "ERROR" "Failed to clone Piper"
        exit 1
    fi
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
    
    # Save new version to state
    save_state "piper" "$REMOTE_PIPER_VERSION"
    log_message "INFO" "Updated Piper state to: $REMOTE_PIPER_VERSION"
fi

# Copy ARM libraries
echo "Setting up libraries..." | tee -a "$LOG_FILE"
ONNX_LIB=$(brew list onnxruntime | grep libonnxruntime | grep dylib | head -n 1)
if [ ! -f "$BIN_DIR/$(basename "$ONNX_LIB")" ]; then
    cp "$ONNX_LIB" "$BIN_DIR/"
    cd "$BIN_DIR"
    ln -sf "$(basename "$ONNX_LIB")" libonnxruntime.1.14.1.dylib
fi

# Function to calculate file checksum
get_file_checksum() {
    local file="$1"
    if [ -f "$file" ]; then
        shasum -a 256 "$file" | cut -d' ' -f1
    else
        echo ""
    fi
}

# Function to verify model checksum
verify_model_checksum() {
    local model_file="$1"
    local model_name="$2"
    
    if [ ! -f "$model_file" ]; then
        return 1
    fi
    
    local current_checksum=$(get_file_checksum "$model_file")
    local stored_checksum=$(echo "$CURRENT_STATE" | jq -r ".components.model_checksums[\"$model_name\"] // \"\"")
    
    if [ -n "$stored_checksum" ] && [ "$current_checksum" = "$stored_checksum" ]; then
        return 0
    fi
    
    return 1
}

# Function to save model checksum
save_model_checksum() {
    local model_file="$1"
    local model_name="$2"
    local checksum=$(get_file_checksum "$model_file")
    
    # Update state with new checksum
    CURRENT_STATE=$(echo "$CURRENT_STATE" | jq \
        --arg name "$model_name" \
        --arg sum "$checksum" \
        '.components.model_checksums[$name] = $sum')
    echo "$CURRENT_STATE" > "$STATE_FILE"
}

# Setup voice models
log_message "INFO" "Setting up voice models..."
cd "$MODELS_DIR"

# Model information
MODEL_NAME="en_US-joe-medium.onnx"
CONFIG_NAME="${MODEL_NAME}.json"
SOURCE_MODEL="/Users/taras/repos/piper-mode/models/$MODEL_NAME"
SOURCE_CONFIG="/Users/taras/repos/piper-mode/models/$CONFIG_NAME"
MODEL_URL="https://huggingface.co/rhasspy/piper-voices/resolve/v1.0.0/en/en_US/joe/medium/$MODEL_NAME"
CONFIG_URL="https://huggingface.co/rhasspy/piper-voices/resolve/v1.0.0/en/en_US/joe/medium/$CONFIG_NAME"

# Check if we need to update models
if [ -f "$SOURCE_MODEL" ] && [ -f "$SOURCE_CONFIG" ]; then
    log_message "INFO" "Checking source models..."
    if ! verify_model_checksum "$SOURCE_MODEL" "$MODEL_NAME"; then
        log_message "INFO" "Copying models from source directory..."
        cp "$SOURCE_MODEL" "$MODELS_DIR/" || { log_message "ERROR" "Failed to copy model file"; exit 1; }
        cp "$SOURCE_CONFIG" "$MODELS_DIR/" || { log_message "ERROR" "Failed to copy config file"; exit 1; }
        save_model_checksum "$MODELS_DIR/$MODEL_NAME" "$MODEL_NAME"
        log_message "INFO" "Updated model checksums"
    else
        log_message "INFO" "Source models are up to date"
    fi
else
    log_message "INFO" "Checking downloaded models..."
    if ! verify_model_checksum "$MODEL_NAME" "$MODEL_NAME" || [ ! -f "$MODEL_NAME" ] || [ "$(stat -f %z "$MODEL_NAME" 2>/dev/null || echo 0)" -lt 1000000 ]; then
        log_message "INFO" "Downloading voice models..."
        curl -L -o "$MODEL_NAME" "$MODEL_URL" || { log_message "ERROR" "Failed to download model"; exit 1; }
        curl -L -o "$CONFIG_NAME" "$CONFIG_URL" || { log_message "ERROR" "Failed to download config"; exit 1; }
        save_model_checksum "$MODEL_NAME" "$MODEL_NAME"
        log_message "INFO" "Updated model checksums"
    else
        log_message "INFO" "Downloaded models are up to date"
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

# Cleanup temporary directories
log_message "INFO" "Cleaning up temporary directories..."
manage_temp_dirs "cleanup" "$TEMP_DIR"
manage_temp_dirs "cleanup" "$BUILD_DIR"
manage_temp_dirs "cleanup" "$ESPEAK_BUILD_DIR"
manage_temp_dirs "cleanup" "$PIPER_BUILD_DIR"
