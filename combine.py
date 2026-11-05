import subprocess

for i in range(1, 100):
    frame = "frame{:04}.png".format(i)
    subprocess.run(["convert", "-append", "trend_plot_full/" + frame, "trend_plot_conditional/" + frame, "trend_plot_combined/" + frame])
