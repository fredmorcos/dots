# Subpixel hinting mode can be chosen by setting the right TrueType interpreter
# version. The available settings are:
#
#     truetype:interpreter-version=35  # Classic mode (default in 2.6)
#     truetype:interpreter-version=40  # Minimal mode (default in 2.7)
#
# There are more properties that can be set, separated by whitespace. Please
# refer to the FreeType documentation for details.

# Uncomment and configure below
#export FREETYPE_PROPERTIES="truetype:interpreter-version=40"
#export FREETYPE_PROPERTIES="cff:no-stem-darkening=0 autofitter:no-stem-darkening=0"
export FREETYPE_PROPERTIES=\
"autofitter:no-stem-darkening=0\
 autofitter:darkening-parameters=500,0,1000,500,2500,500,4000,0\
 cff:no-stem-darkening=0\
 cff:darkening-parameters=500,475,1000,475,2500,475,4000,0\
 type1:no-stem-darkening=0\
 type1:darkening-parameters=500,475,1000,475,2500,475,4000,0\
 t1cid:no-stem-darkening=0\
 t1cid:darkening-parameters=500,475,1000,475,2500,475,4000,0"
