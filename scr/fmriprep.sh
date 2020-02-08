#!/bin/sh

# This is a BIAC template script for jobs on the cluster
# You have to provide the Experiment on command line
# when you submit the job the cluster.
#
# >  qsub -v EXPERIMENT=Dummy.01  script.sh args
#
# There are 2 USER sections
#  1. USER DIRECTIVE: If you want mail notifications when
#     your job is completed or fails you need to set the
#     correct email address.
#
#  2. USER SCRIPT: Add the user script in this section.
#     Within this section you can access your experiment
#     folder using $EXPERIMENT. All paths are relative to this variable
#     eg: $EXPERIMENT/Data $EXPERIMENT/Analysis
#     By default all terminal output is routed to the " Analysis "
#     folder under the Experiment directory i.e. $EXPERIMENT/Analysis
#     To change this path, set the OUTDIR variable in this section
#     to another location under your experiment folder
#     eg: OUTDIR=$EXPERIMENT/Analysis/GridOut
#     By default on successful completion the job will return 0
#     If you need to set another return code, set the RETURNCODE
#     variable in this section. To avoid conflict with system return
#     codes, set a RETURNCODE higher than 100.
#     eg: RETURNCODE=110
#     Arguments to the USER SCRIPT are accessible in the usual fashion
#     eg:  $1 $2 $3
# The remaining sections are setup related and don't require
# modifications for most scripts. They are critical for access
# to your data

# --- BEGIN GLOBAL DIRECTIVE --
#$ -S /bin/sh
#$ -o $HOME/$JOB_NAME.$JOB_ID.out
#$ -e $HOME/$JOB_NAME.$JOB_ID.out
# -- END GLOBAL DIRECTIVE --

# -- BEGIN PRE-USER --
EXPERIMENT=${EXPERIMENT:?"Experiment not provided"}

source /etc/biac_sge.sh

EXPERIMENT=`findexp $EXPERIMENT`
EXPERIMENT=${EXPERIMENT:?"Returned NULL Experiment"}

if [ $EXPERIMENT = "ERROR" ]
then
	exit 32
else
#Timestamp
echo "----JOB [$JOB_NAME.$JOB_ID] START [`date`] on HOST [$HOSTNAME]----"
# -- END PRE-USER --
# **********************************************************

# -- BEGIN USER DIRECTIVE --
# Send notifications to the following address ###CHANGE THIS AND REMOVE # (optional)
$ -M kls77@duke.edu

# -- END USER DIRECTIVE --

# -- BEGIN USER SCRIPT --
# User script goes here
SUB=$1

###CHANGE TO YOUR BIDS BASE DIRECTORY (dont include $sub at the end)
DATADIR=${EXPERIMENT}/Data/MRI_Data/bids
###CHANGE TO YOUR FREESURFER LICENSE FILE
FSFILE=${EXPERIMENT}/Data/MRI_Data
###CHANGE TO DESIRED OUTPUT
OUTPUT=${EXPERIMENT}/Data/MRI_Data/derivatives

###CHANGE WHERE YOU WANT THE WORKFILES TO GO
WORK=${EXPERIMENT}/Data/MRI_Data/derivatives/work/


singularity run --cleanenv /usr/local/packages/singularity/images/fmriprep.simg \
${DATADIR} ${OUTPUT} \
participant \
--participant-label ${SUB} \
--fs-no-reconall \
--fs-license-file ${FSFILE} \
--ignore fieldmaps \
--use-syn-sdc \
--output-space T1w template \
--template-resampling-grid 2mm \
--nthreads 2 \
--omp-nthreads 4 \
--mem-mb 16000 \
-w ${WORK}
#--low-mem \
#-w ${WORK} ###

OUTDIR=${EXPERIMENT}/Data/MRI_Data/derivatives/Logs
#OUTDIR=${EXPERIMENT}/Analysis/fmriprep/Logs
mkdir -p $OUTDIR

echo "----JOB [$JOB_NAME.$JOB_ID] STOP [`date`]----"
#OUTDIR=${OUTDIR:-$EXPERIMENT/Analysis}
mv $HOME/$JOB_NAME.$JOB_ID.out $OUTDIR/$JOB_NAME.$JOB_ID.out
RETURNCODE=${RETURNCODE:-0}
exit $RETURNCODE
fi
