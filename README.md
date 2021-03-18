# plotter
Scripts to plot mutation spectra in 96 or 192 channel format, using base R. Header is important to ensure everything is ordered correctly. RefHeight and AltHeight vary by aspect ratio, hence the options to help centre them in the colour boxes. Use labs=0 if you want to remove the colour boxes and labels.

Example time. Lets plot a 192 channel mutational spectrum.
<pre>
par(mar=c(1,5,2,2)) ; plotSpectrum192(signature[1,],bc=baseCol,labs = 1,yUpperBound = 0.07, yLowerBound = 0,labScale = 1,ylab="mu/Mb",RefHeight=-1.1,AltHeight=-2.15)
</pre>

![alt text](https://github.com/CraigJAnderson/plotter/blob/main/example_192_spectra.jpeg)

Let's plot a 96 channel mutational spectrum with confidence intervals 
<pre>par(mar=c(1,5,2,2)) ; plotSpectrum96(signature[1,],signature_error_bars[1,],bc=baseCol,labs = 1,yUpperBound = 0.11, yLowerBound = 0,labScale = 1,ylab="mu/Mb",RefHeight=-1.1,AltHeight=-1.9) </pre>
![alt text](https://github.com/CraigJAnderson/plotter/blob/main/example_96_spectra.jpeg)

If you use my scripts in your amazing nature paper, then I need to be the corresponding author ;-)
