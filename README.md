# plotter
plot 192 channel mutational spectra, with options for confidence intervals

scripts to plot lovely mutation spectra in 96 or 192 channel format, using base R. Header is important to ensure everything is ordered correctly. RefHeight and AltHeight vary by aspect ratio, hence the options to help centre them in the colour boxes. Use labs=0 if you want to remove the colour boxes and labels.
<pre>
par(mar=c(1,5,2,2)) ; plotSpectrum192(signature[1,],signature_error_bars[1,],bc=baseCol,labs = 1,yUpperBound = 0.1, yLowerBound = 0,labScale = 1,ylab="mu/Mb",RefHeight=-1.1,AltHeight=-2.15)
</pre>
