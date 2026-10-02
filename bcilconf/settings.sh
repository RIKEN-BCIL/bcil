export CARET7DIR=/mnt/pub/devel/workbench/release/1.5.0
export HCPPIPEDIR=/mnt/pub/devel/NHPHCPPipeline
export FREESURFER_HOME=/usr/local/freesurfer-v5.3.0-HCP


# --- MATLAB execution mode for the QC plotting steps -------------------------
# "matlab"  : use a live MATLAB session (default; needs a MATLAB license)
# "runtime" : use the compiled standalone binaries under $BCILDIR/bin/compiled/
#             running on the MATLAB Runtime (no MATLAB license needed).
#             Build them first with $BCILDIR/bin/compile_qc_matlab.sh
export MATLAB_MODE=${MATLAB_MODE:-matlab}
# MATLAB Runtime root, required only when MATLAB_MODE=runtime. Point to an
# installed MCR (e.g. /usr/local/MATLAB/MATLAB_Runtime/v913 for R2022b) or to a
# full MATLAB install root compiled with the matching version.
export MCRROOT=${MCRROOT:-}
