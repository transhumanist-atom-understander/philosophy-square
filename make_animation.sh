Rscript cause_interference_animation.R
python3 combine.py
# Following instructions from here:
# https://stackoverflow.com/questions/30927367/imagemagick-making-2-gifs-into-side-by-side-gifs-using-im-convert
convert -loop 0 -delay 3 trend_plot_combined/*.png trend_plot_combined.gif
