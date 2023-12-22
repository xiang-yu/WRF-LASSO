## AFTER execute the "configure.wrf", remember to repalce "openmp" by "qopenmp" at "OMP =  -openmp -fpp -auto"
## and "OMPCC = -openmp -fpp -auto"; Also, change "FC = time $(DM_FC)" to "FC = $(DM_FC)"
module purge
module load intel/20.0.4
module load mvapich2/2.3.6
module load netcdf/4.8.0
export NETCDF=/share/apps/netcdf/4.8.0/intel/20.0.4/
export WRFIO_NCD_LARGE_FILE_SUPPORT=1

ulimit -s unlimited
#./compile em_crm >& log_DM_FC &
#./compile em_crm >& log_kappa &
#./compile em_crm >& log_kappa_fix &
#./compile em_crm >& log_kappa_DUM &
./compile em_crm >& log_kappa_sig &
