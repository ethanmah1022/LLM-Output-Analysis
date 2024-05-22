# LLM Output Analysis

This Shiny application provides various analyses for outputs generated by different Large Language Models (LLMs) during text-generating and editing tasks. The app includes features for sentiment analysis, frequency analysis, lexical diversity, and author similarity. This application generates more suitable workplace communication messages from initial drafts that may be inappropriate in tone or structure. It uses different LLMs to create alternative messages and provides tools to analyze and compare these outputs based on various linguistic and sentiment metrics.

## Features

- **Sentiment Analysis**: Utilize the `quanteda.sentiment` library to assess the sentiment of the texts.
- **Frequency Analysis**: Examine the frequency of words and phrases used in the texts.
- **Lexical Diversity**: Measure the variety of vocabulary used in the texts.
- **Author Similarity**: Compare the style and structure of the texts to the original message and between LLM outputs.

## Prerequisites

Make sure you have the following R libraries installed:

- `shiny`
- `shinydashboard`
- `tidyverse`
- `tidytext`
- `quanteda`
- `quanteda.textstats`
- `quanteda.textplots`
- `quanteda.corpora`
- `quanteda.sentiment`
- `ggplot2`

You can install them using:

```r
install.packages(c("shiny", "shinydashboard", "tidyverse", "tidytext", "quanteda", 
                   "quanteda.textstats", "quanteda.textplots", "quanteda.corpora", 
                   "quanteda.sentiment", "ggplot2"))

## Credits
This application makes use of the `quanteda.sentiment` library for sentiment analysis functionality. You can find more information about the library here.

For users who want to learn more about quanteda functions, you can refer to the following resources:
- [Quanteda Tutorial](https://quanteda.io/tutorials/)
- [Quanteda Sentiment GitHub Repository](https://github.com/quanteda/quanteda.sentiment)

## License
This project is licensed under the MIT License. See the LICENSE file for details.
