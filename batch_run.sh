#!/bin/bash
# filepath: /home/lev/DOCTORATE/Cytomata/batch_run.sh

set -e

usage() {
    echo "Usage: $0 -cytomata /path/to/cytomata -data /path/to/cytomata_data -projects project1 project2 ..."
    exit 1
}

if [ "$#" -lt 1 ]; then
    usage
fi

while [[ $# -gt 0 ]]; do
    key="$1"
    case $key in
        -cytomata)
            CYTOMATA_DIR="$2"
            shift 2
            ;;
        -data)
            DATA_DIR="$2"
            shift 2
            ;;
        -projects)
            shift
            PROJECTS=()
            while [[ $# -gt 0 && $1 != -* ]]; do
                PROJECTS+=("$1")
                shift
            done
            ;;
        *)
            echo "Unknown option: $1"
            usage
            ;;
    esac
done

if [ -z "$CYTOMATA_DIR" ] || [ -z "$DATA_DIR" ] || [ ${#PROJECTS[@]} -eq 0 ]; then
    echo "Error: Missing required parameters."
    usage
fi

for project in "${PROJECTS[@]}"; do
    echo "Processing project: $project"
    SETTINGS_SRC="${DATA_DIR}/${project}/settings.xlsx"
    SETTINGS_DEST="${CYTOMATA_DIR}/settings.xlsx"

    if [ ! -f "$SETTINGS_SRC" ]; then
        echo "Error: settings.xlsx not found at ${SETTINGS_SRC}"
        exit 1
    fi

    cp "$SETTINGS_SRC" "$SETTINGS_DEST"
    echo "Copied settings.xlsx from ${SETTINGS_SRC} to ${SETTINGS_DEST}"

    # Run master.R non-interactively using Rscript
    cd "$CYTOMATA_DIR"
    Rscript master.R


    echo "Finished processing project: $project"
done