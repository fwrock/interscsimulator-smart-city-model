rm -fr src/Sim-Diasca*
rm -f src/*.traces

cd src
make all -j 8 EXECUTION_TARGET=production &&\
 EXECUTION_TARGET=production\
 CONFIG_PATH="../sp_simplificado/config.xml"\
 make smart_city_run CMD_LINE_OPT="--batch"
