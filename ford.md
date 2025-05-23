project: moonframe
src_dir: ./src
output_dir: ./doc
media_dir: ./media
project_github: https://github.com/jacobwilliams/moonframe
summary: A Fortran library to interpolate the MOON_PA reference frame without using SPICE.
author: Jacob Williams
github: https://github.com/jacobwilliams
predocmark_alt: >
predocmark: <
docmark_alt:
docmark: !
display: public
         private
         protected
source: true
graph: true
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
            bspline_module:https://jacobwilliams.github.io/bspline-fortran/
            csv_module:https://jacobwilliams.github.io/csv-fortran/

{!README.md!}