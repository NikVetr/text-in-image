# Text-in-Image

## Introduction

You've seen generative text-to-image models, but why not try something substantially less sophisticated? Text-in-Image is an R-based tool designed to automate text-wrapping and optimization from provided text and image input files. I wrote it to create posters and other decorative items for friends, like this one from 2023 I'd made as a graduation presented (printed and signed by the lab):

![Generated {Poster}](output/img/graduation_rat_.jpg)

It's quite flexible and features many optional pre- and post-processing steps, such noise removal, shape and color simplification (extending another program I'd written, `raster-to-vector`), and adaptive color adjustment. The basic workflow involves identifying valid regions where text may be written, fitting text in those regions, and iteratively scaling the text size until convergence. 

I've included an example script in `/R/example.R` that takes the first chapter of Lewis Caroll's Alice in Wonderland and writes it in one of its illustrations:


You'll see the exploded workflow for how this result is obtained below.

[TO BE CONTINUED]

## Features

## Getting Started

## Prerequisites

## Installation

## Example

## Contributing

## Acknowledgments
