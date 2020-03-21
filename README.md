# ChromaR Documentation

ChromaR is an experimental R toolkit for analyzing and exploring chromatic storytelling in movies or any other video source. 

*This documentation assumes that the reader is already familiar with R. If not so, a gentle introduction to R can be found [here](https://rpubs.com/pjmurphy/414993/).*

<p align="center">
  <img src="https://miro.medium.com/max/7680/1*fDmp6lWqmpQXyEb8KYYQaA.png" alt="frameline example" width="100%"/>
  <p align ="center"><small>Examples of framelines</small></p>
</p>

## Summary

* [Getting Started](https://github.com/detsutut/chroma#getting-started)
* [Generate your own dataset](https://github.com/detsutut/chroma#generate-your-own-dataset)
* [Built With](https://github.com/detsutut/chroma#built-with)
* [Authors](https://github.com/detsutut/chroma#authors)
* [Acknowledgements](https://github.com/detsutut/chroma#acknowledgements)
* [License](https://github.com/detsutut/chroma#license)

## Getting Started

### Installing ChromaR

The ChromaR package is hosted on GitHub and must be therefore installed through Devtools:

```r
> install.packages("devtools")
> library(devtools)
> install_github("detsutut/chroma", subdir="chromaR")
> library(chromaR)
```

### Inspecting Default Datasets

ChromaR comes with two ready-made lists of datasets: Matrix and Miyazaki. These lists collect the movie datasets for the Matrix trilogy and Miyazaki's filmography. Let's look at "Princess Mononoke":

```r
> miyazaki = chromaR::miyazaki
> mononoke = miyazaki[[5]]
> attr(mononoke, "title")

"1997 - Princess Mononoke"

> mononoke[sample(nrow(mononoke),5),]

frameId R       G       B       hex     lum      
99055   112.930 125.730 95.100  #707D5F 118.52070
157672  62.832  54.164  57.341  #3E3639 57.11387 
80875   66.538  77.007  84.124  #424D54 74.64917  
19967   56.969  106.630 111.510 #386A6F 92.26850 
159740  44.664  49.680  60.627  #2C313C 49.37937
```

Here we see 5 random rows from the Mononoke dataset. Each row represents a single frame of the movie. For each frame, we collect the average R, G and B value, its Hex string equivalent and the average luminance.

### Plotting the First Frameline

In order to draw a nice plot for Mononoke, we must merge the frames together.

```r
> mononoke_grouped = groupframes(mononoke, seconds = 10)
> plotFrameline(mononoke_grouped, verbose = 1)
```

<p align="center">
  <img src="src/mononoke.png" alt="mononoke frameline" width="100%"/>
  <p align ="center"><small>Mononoke</small></p>
</p>

## Generate Your Own Dataset

If you want to explore your own clips, you must convert your source from video to collection of averaged pixels.
To do this, you need to run the following Matlab script

```matlab
[names,path] = uigetfile({'*.mp4;*.m4v;*.3gp;*.mov;*.wmv;*.avi;*.webm;*.mkv;*.flv;*.mpeg;*.mpg',...
    'Video files (*.mp4,*.avi,*.mkv,...)';'*.*',  'All Files (*.*)'},...
    'Frames Extractor - select video files','MultiSelect','on');
if iscell(names)==0
    name = names;
    tot = 1;
else
    tot = length(names);
end
for i = 1:tot
    if iscell(names)~=0
        name = names{i};
    end
    waittitle = strcat(name,' selected (',num2str(i),' out of',' ',num2str(tot),')');
    disp(waittitle);
    path_input = strcat(path,name);
    path_output = strcat(path,name,'.csv');
    mov = VideoReader(path_input);
    nof = floor(mov.duration*mov.FrameRate);
    palette = zeros(nof,3);
    k=0;
    waitmsg = strcat(num2str(k),'/',num2str(nof));
    f = waitbar(0,waitmsg,'Name',waittitle);
    for k = 3 : length(palette)
        if hasFrame(mov)==0
            break;
        end
        frame = readFrame(mov);
        rgb = mean(reshape(frame,[],size(frame,3)));
        palette(k,1)=rgb(1);
        palette(k,2)=rgb(2);
        palette(k,3)=rgb(3);
        waitmsg = strcat(num2str(k),'/',num2str(nof));
        waitbar(k/nof,f,waitmsg);
    end
    close(f)
    csvwrite(path_output,palette)
end
```
This means that **you must have Matlab installed** on your local machine.

## Built With

* [R](https://www.r-project.org/) - Main Language
* [MATLAB](https://it.mathworks.com/products/matlab.html) - From video source to csv

## Authors

* **Tommaso Buonocore** - *Author and Repository Maintainer* - [GitHub](https://github.com/detsutut), [LinkedIn](https://www.linkedin.com/in/tbuonocore/)

## Acknowledgements

Thanks for the inspiration to:

* https://thecolorsofmotion.com/
* http://moviebarcode.tumblr.com/

## License

This project is licensed under the GNU GPLv3 License - see the [LICENSE.md](LICENSE.md) file for details
