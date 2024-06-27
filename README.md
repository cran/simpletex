
# Introduction

By calling the 'SimpleTex' <https://simpletex.cn/> open API implements text and mathematical formula recognition on the image, and the output formula can be used directly with Markdown and LaTeX.

## Installation

``` r
# Install development version from GitLab
remotes::install_gitlab("chuxinyuan/simpletex")
# Install from CRAN
install.packages("simpletex")
```

## Configure the ID and SECRET for the SimpleTex API

To use the simpletex package, user need to first register with the [SimpleTex platform](https://simpletex.cn/), and then create application. After creating the application, Then you will get SimpleTex API ID and SECRET. 

Put your SimpleTex API ID and SECRET in the following code and run it once.

``` r
cat(
  '\n# ID and SECRET of SimpleTex',
  'SIMPLETEX_APP_ID = "Your SimpleTex API ID"',
  'SIMPLETEX_APP_SECRET = "Your SimpleTex API SECRET"',
  file = '~/.Renviron', sep = '\n', append = TRUE
)
```

## Usage

``` r
imgocr(img = "path/to/image", mode = "latex_ocr")
```

- `img` An image file, supporting jpg, png, bmp format.
- `mode` is service model. The value can be `latex_ocr`, `latex_ocr_turbo`, or `simpletex_ocr`. The default value is `latex_ocr`. The `latex_ocr` and `latex_ocr_turbo` are used for formula identification. `latex_ocr` is better than `latex_ocr_turbo`, but `latex_ocr_turbo` is faster. `simpletex_ocr` is suitable for general image recognition.

For the output, for example: 'E=mc^2', you can copy only the parts within
quotation marks of the output directly into the '.md' or '.tex' document.

Note: in order to the formula to render properly in '.md' or '.tex' documents,
you also need to wrap the formula with '$' or '$$' on both sides.

## License

simpletex is free and open source software, licensed under MIT + file LICENSE.
