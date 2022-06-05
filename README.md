# BCIL
Brain Connectomics Imaging Libraries


## Installation
1. Donwload zip file and unzip the file
2. Configure the setting file ([unzipped directory]/bcilconf/settings.sh) for your environments

Default settings are:
```
export CARET7DIR=/mnt/pub/devel/workbench/release/1.5.0
export HCPPIPEDIR=/mnt/pub/devel/NHPHCPPipeline
export FREESURFER_HOME=/usr/local/freesurfer-v5.3.0-HCP
export FSLDIR=/usr/local/fsl
```
Please edit the paths using *nano* (or whatever editor you like) for your environments. CARET7DIR is the path to Workbench, HCPPIPEDIR is the path to HCP pipeline, FREESURFER_HOME is the path to FreeSurfer.

3. Set path to [unzipped directory]/bin
```
export PATH=[unzipped directory]/bin:$PATH
```
4. Run commands in the directory, [unzipped directory]/bin

