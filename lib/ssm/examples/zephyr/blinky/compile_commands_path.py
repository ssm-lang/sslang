# Some dark magic/ugly hack to put compile_commands.json in the right place,
# from https://docs.platformio.org/en/latest//integration/compile_commands.html
#
# I really hope they work around this mad trip of a script/hack (what the hell
# is `Import`??) and actually support configuring this from platformio.ini.
import os
Import("env")

env.Replace(COMPILATIONDB_PATH=os.path.join("$PROJECT_DIR", "compile_commands.json"))
