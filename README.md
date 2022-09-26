# BCIL
Brain Connectomics Imaging Libraries

## Installation
1. Requires FSL, WORKBENCH, FreeSurfer, matlab, R
matlab toolbox: DSE decomposition https://github.com/asoroosh/DVARS
R toolbox: ggplot2, ggQC, gridExtra

2. Donwload zip file of bcil and unzip the file
3. Configure the setting file ([unzipped directory]/bcilconf/settings.sh) for your environments

Default settings are:
```
export CARET7DIR=/mnt/pub/devel/workbench/release/1.5.0
export HCPPIPEDIR=/mnt/pub/devel/NHPHCPPipeline
export FREESURFER_HOME=/usr/local/freesurfer-v5.3.0-HCP
export FSLDIR=/usr/local/fsl
export DVARSDIR=/mnt/pub/devel/git/DVARS-master
```
Please edit the paths using *nano* (or whatever editor you like) for your environments. CARET7DIR is the path to Workbench, HCPPIPEDIR is the path to HCP pipeline, FREESURFER_HOME is the path to FreeSurfer.

4. Set path to [unzipped directory]/bin
```
export PATH=[unzipped directory]/bin:$PATH
```
5. Run commands in the directory, [unzipped directory]/bin


