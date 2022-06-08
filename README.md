# BCIL

Brain Connectomics Imaging Libraries


## Installation
1. Donwload zip file and unzip the file
2. Configure the setting file ([unzipped directory]/bcilconf/settings.sh) for environments

Default setting is:
```
export CARET7DIR=/mnt/pub/devel/workbench/release/1.5.0
export HCPPIPEDIR=/mnt/pub/devel/NHPHCPPipeline
export FREESURFER_HOME=/usr/local/freesurfer-v5.3.0-HCP
```
Please edit this file depending on your environments

3. Set path to [unzipped directory]/bin
```
export PATH=[unzipped directory]/bin:$PATH
```
4. Run commands in the directory, [unzipped directory]/bin

## Dependencies
Workbench (1.5.0 or higher), HCP pipeline (3.6 or higher), FreeSurfer(5.3 or higher), ImageMagick

## License
BCIL is licensed under the terms of the MIT license.
