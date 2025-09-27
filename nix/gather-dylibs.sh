#!/usr/bin/env bash

set -euo pipefail

# Script to recursively gather all dylib dependencies and package them alongside
# a binary

if [ $# -ne 3 ]; then
    echo "Usage: $0 <binary_or_dylib> <target_dir> <target_prefix>"
    echo "Example: $0 /path/to/binary /path/to/output/dir" "@executable_path/"
    exit 1
fi

SOURCE_FILE="$1"
TARGET_DIR="$2"
FINAL_PREFIX="$3"

# Create target directory if it doesn't exist
mkdir -p "$TARGET_DIR"

# Track processed files to avoid infinite loops
declare -A processed_files
declare -A copied_files

# Libraries that are guaranteed to be available on macOS and should use system versions
declare -A system_libs
system_libs["libiconv.2.dylib"]="/usr/lib/libiconv.2.dylib"
system_libs["libz.dylib"]="/usr/lib/libz.1.dylib"
system_libs["libresolv.9.dylib"]="/usr/lib/libresolv.9.dylib"
# system_libs["libgssapi_krb5.2.2.dylib"]="/usr/lib/libgssapi_krb5.2.2.dylib"
# system_libs["libk5crypto.3.1.dylib"]="/usr/lib/libk5crypto.3.1.dylib"
# system_libs["libkrb5.3.3.dylib"]="/usr/lib/libkrb5.3.3.dylib"
# system_libs["libkrb5support.1.1.dylib"]="/usr/lib/libkrb5support.1.1.dylib"
# system_libs["libcom_err.3.0.dylib"]="/usr/lib/libcom_err.3.0.dylib"

get_dylib_basename() {
    local path="$1"
    basename "$path"
}

gather_deps() {
    local file="$1"
    local file_basename
    file_basename=$(basename "$file")

    if [[ -n "${processed_files[$file]:-}" ]]; then
        return 0
    fi
    processed_files["$file"]=1

    echo "Processing: $file"

    local deps
    deps=$(otool -L "$file" 2>/dev/null | tail -n +2 | awk '{print $1}' | grep -v "^$file" || true)

    for dep in $deps; do
        if [[ "$dep" =~ ^/usr/lib/ ]] || [[ "$dep" =~ ^/System/ ]]; then
            echo "  Skipping system lib: $dep"
            continue
        fi

        if [[ "$dep" =~ ^@(rpath|loader_path|executable_path)/ ]]; then
            echo "  Skipping relative path: $dep"
            continue
        fi

        if [[ "$dep" =~ ^/nix/store/ ]]; then
            local dep_basename
            dep_basename=$(get_dylib_basename "$dep")

            if [[ -n "${system_libs[$dep_basename]:-}" ]]; then
                echo "  Skipping system lib (will use ${system_libs[$dep_basename]}): $dep_basename"
                continue
            fi

            local target_path="$TARGET_DIR/$dep_basename"

            if [[ -z "${copied_files[$dep]:-}" ]]; then
                if [[ -f "$dep" ]]; then
                    echo "  Copying: $dep -> $target_path"
                    cp -L "$dep" "$target_path"
                    copied_files["$dep"]=1

                    gather_deps "$target_path"
                else
                    echo "  WARNING: File not found: $dep"
                fi
            else
                echo "  Already copied: $dep_basename"
            fi
        else
            echo "  Non-Nix dependency: $dep"
        fi
    done
}

echo "Gathering dylib dependencies for: $SOURCE_FILE"
echo "Target directory: $TARGET_DIR"
echo "----------------------------------------"

gather_deps "$SOURCE_FILE"

echo "----------------------------------------"
echo "Fixing dylib references..."

# Fix references in the main binary and all copied dylibs
all_files=("$SOURCE_FILE")
for copied_file in "${!copied_files[@]}"; do
    dylib_basename=$(get_dylib_basename "$copied_file")
    all_files+=("$TARGET_DIR/$dylib_basename")
done

for file in "${all_files[@]}"; do
    echo "Fixing references in $file"

    # For dylibs copied to TARGET_DIR, also fix their install name (first line of otool -L output)
    if [[ "$file" =~ ^"$TARGET_DIR"/ ]]; then
        install_name=$(otool -L "$file" 2>/dev/null | head -n 2 | tail -n 1 | awk '{print $1}')
        if [[ "$install_name" =~ ^/nix/store/ ]]; then
            file_basename=$(basename "$file")
            echo "  Fixing install name: $install_name -> $file_basename"
            install_name_tool -id "$file_basename" "$file"
        fi
    fi

    # Find all Nix store references and fix them
    nix_refs=$(otool -L "$file" 2>/dev/null | tail -n +2 | awk '{print $1}' | grep "^/nix/store/" || true)

    for nix_ref in $nix_refs; do
        ref_basename=$(basename "$nix_ref")

        # Check if this should use a system library instead
        if [[ -n "${system_libs[$ref_basename]:-}" ]]; then
            echo "  Changing $nix_ref -> ${system_libs[$ref_basename]}"
            install_name_tool -change "$nix_ref" "${system_libs[$ref_basename]}" "$file"
        # Only fix if we have a local copy
        elif [[ -f "$TARGET_DIR/$ref_basename" ]]; then
            echo "  Changing $nix_ref -> ${FINAL_PREFIX}${ref_basename}"
            install_name_tool -change "$nix_ref" "${FINAL_PREFIX}${ref_basename}" "$file"
        fi
    done
done

echo "----------------------------------------"
echo "Dependency gathering and fixing complete!"
echo "Copied dylibs:"
for copied_file in "${!copied_files[@]}"; do
    echo "  $(get_dylib_basename "$copied_file")"
done

echo ""
echo "----------------------------------------"
echo "Verifying no Nix store references remain..."

check_no_nix_refs() {
   local executable="$1"

   if otool -L "$executable" | tail -n +2 | grep -q "/nix/store/"; then
       echo "ERROR: $executable still contains Nix store references:"
       otool -L "$executable" | tail -n +2 | grep "/nix/store/"
       return 1
   fi
}

verification_failed=false
for file in "${all_files[@]}"; do
    echo "Checking $file:"
    if ! check_no_nix_refs "$file"; then
        verification_failed=true
    else
        echo "  OK: No Nix store references found"
    fi
done

if [ "$verification_failed" = true ]; then
    echo ""
    echo "ERROR: Some files still contain Nix store references!"
    exit 1
else
    echo ""
    echo "SUCCESS: All files are clean of Nix store references!"
fi
