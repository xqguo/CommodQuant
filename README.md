# CommodQuant

## Project Overview

CommodQuant is an open-source F# library designed for quantitative analysis in the commodity markets. It provides a comprehensive toolkit for professionals and researchers working with commodity derivatives, offering functionalities for pricing complex structures, managing risk, and analyzing market data. The library aims to deliver robust and efficient financial computations leveraging the strengths of the F# language.

## Table of Contents

- [Features](#features)
- [Getting Started](#getting-started)
  - [Prerequisites](#prerequisites)
  - [Installation](#installation)
  - [Basic Configuration](#basic-configuration)
- [Usage](#usage)
  - [Referencing the Library](#referencing-the-library)
  - [Core Concepts (Briefly)](#core-concepts-briefly)
  - [Basic Examples](#basic-examples)
- [Running Tests](#running-tests)
- [Documentation](#documentation)
- [Contributing](#contributing)
- [License](#license)

## Features

*   **Commodity Derivative Pricing**: Supports pricing for a range of commodity derivatives, including:
    *   Vanilla and exotic options (e.g., Asians, Spreads).
    *   Average price swaps.
*   **Advanced Option Pricing Models**: Implements several sophisticated models for accurate option valuation:
    *   Choi's method for multi-factor Asian and spread options.
    *   Gabillon two-factor model for commodity term structure and volatility.
    *   Moment-matching techniques for approximating complex option payoffs.
*   **Monte Carlo Simulation**: Includes capabilities for pricing path-dependent options and performing other stochastic simulations.
*   **Volatility Modeling**: Offers tools for advanced volatility analysis:
    *   Construction and interpolation of volatility smiles (e.g., using cubic splines).
    *   SVI (Stochastic Volatility Inspired) model for smile parameterization and fitting.
*   **Interest Rate Curve Handling**: Facilitates the construction and usage of interest rate curves (e.g., USD OIS) for discounting, leveraging QuantLib-Net.
*   **Market Data Management**: Provides robust tools for handling market data:
    *   Loading price curves, volatility surfaces, and historical fixings from CSV files.
    *   Sophisticated logic for contract expiry dates, holiday calendars, and instrument-specific conventions across various commodities.
*   **Quantitative Finance Utilities**: A collection of mathematical functions (e.g., numerical integration via Gauss-Hermite quadrature, normal distribution functions) and specialized financial utility functions.
*   **Excel Interoperability**: Utilities to read data from and write data to Microsoft Excel, facilitating integration with existing spreadsheet-based workflows.

## Getting Started

This section will guide you through setting up the CommodQuant library on your local machine.

### Prerequisites

*   **.NET SDK**: Ensure you have the .NET SDK installed. The latest stable version is recommended. You can download it from [here](https://dotnet.microsoft.com/download). F# is included with the .NET SDK.
*   **IDE/Text Editor**: An F# compatible development environment such as:
    *   Visual Studio
    *   Visual Studio Code with the [Ionide extension](https://ionide.io/)
    *   JetBrains Rider

### Installation

1.  **Clone the Repository**:
    The primary way to get CommodQuant is by cloning the repository:
    ```bash
    git clone https://github.com/xqguo/CommodQuant.git
    cd CommodQuant
    ```

2.  **Build the Solution**:
    You can build the library using the .NET CLI:
    ```bash
    dotnet build src/CommodQuant.sln
    ```
    This will compile the library, and the resulting DLL (`CommodLib.dll`) will typically be found in a subdirectory like `src/Library/bin/Debug/netX.X/` (the exact path depends on your .NET version and build configuration).

### Basic Configuration

*   **Data Files**: The library often relies on CSV files for market data (prices, volatilities, contract specifications) located in the `src/Library/csv` and `src/Library/holidays` directories. If you are running examples or using the library in a way that reads these files, ensure that the `ROOT` path variable in `src/Library/IO.fs` correctly points to the directory containing these `csv` and `holidays` folders, or adjust paths as needed in your own configuration. By default, it attempts to auto-detect this based on the DLL's location.

## Usage

This section provides a basic guide on how to use the CommodQuant library in your F# projects or scripts.

### Referencing the Library

*   **From an F# Script (`.fsx`)**:
    Once you have built the library, you can reference the `CommodLib.dll` in your F# scripts:
    ```fsharp
    // Ensure the path to the DLL is correct for your build output
    #r "path/to/src/Library/bin/Debug/net6.0/CommodLib.dll" // Example for .NET 6
    open Commod // Open the main namespace

    // Your script code here
    printfn "CommodQuant library referenced!"
    ```
    Adjust the path in `#r` based on your .NET SDK version and build configuration (Debug/Release).

*   **From an F# Project (`.fsproj`)**:
    To use CommodQuant in another F# project, add a project reference to `CommodLib.fsproj`:
    ```xml
    <ItemGroup>
        <ProjectReference Include="path/to/src/Library/CommodLib.fsproj" />
    </ItemGroup>
    ```
    Or, if you are referencing the DLL directly:
    ```xml
    <ItemGroup>
        <Reference Include="CommodLib">
            <HintPath>path/to/src/Library/bin/Debug/net6.0/CommodLib.dll</HintPath> <!-- Example for .NET 6 -->
        </Reference>
    </ItemGroup>
    ```

### Core Concepts (Briefly)

*   **`Instrument` (in `DomainTypes.fs`)**: Represents different commodity types (e.g., `BRT` for Brent, `NG` for Natural Gas).
*   **`Commod` (in `DomainTypes.fs`)**: An object holding all relevant information for a specific commodity, including its calendar, contract dates, quotation units, and lot sizes. Typically obtained via `Markets.getCommod Instrument`.
*   **`PriceCurve`, `VolCurve` (in `DomainTypes.fs`)**: Represent market price and volatility data, loaded via functions in the `Markets` module.
*   **Pricers (in `Pricer.fs`)**: Various functions dedicated to pricing specific derivatives (e.g., `AsianOptionPricer`, `SpreadOptionPricerBS`).
*   **Option Definitions**: Parameters like strike, expiration, and payoff type (`Call` or `Put` from `DomainTypes.fs`).

### Basic Examples

1.  **Loading Market Data**:
    This example shows how to load a price curve for Brent (BRT) crude oil. Ensure your CSV files are correctly located relative to where the library expects them (see Basic Configuration).

    ```fsharp
    open System
    open Commod

    // If running from a script outside the main project structure,
    // you might need to help the library find its data files:
    // IOcsv.ROOT <- "path/to/your/CommodQuant/src/Library" // Adjust if needed and uncomment

    let pricingDate = DateTime.Today

    // Get the commodity definition for Brent
    let brtCommodity = Markets.getCommod BRT

    // Load the price curve for BRT
    // This assumes a BRT_Price.csv file exists in the configured csv directory
    try
        let brtPriceCurve = Markets.getPrices BRT
        printfn "Successfully loaded BRT price curve."
        // You can inspect the curve, for example, by printing a specific pillar's price
        // (Assuming "JAN-25" is a valid pillar in your CSV data)
        match brtPriceCurve.Observations |> Map.tryFind "JAN-25" with
        | Some price -> printfn "Price for JAN-25: %A" price
        | None -> printfn "Pillar JAN-25 not found in the curve."
    with
    | ex -> printfn "Error loading BRT price curve: %s" ex.Message
    ```

2.  **Pricing a Simple European Option (Black-Scholes)**:
    This example demonstrates pricing a simple European call option using the Black-Scholes formula from the `Options` module.

    ```fsharp
    open System
    open Commod // Access to Options.bs and DomainTypes.Payoff

    // Option parameters
    let forwardPrice = 100.0  // Current forward price of the underlying asset
    let strikePrice = 105.0   // Strike price of the option
    let timeToExpiry = 0.5    // Time to expiration in years (e.g., 6 months)
    let volatility = 0.20     // Implied volatility (e.g., 20%)
    let optionType = Payoff.Call // Or Payoff.Put for a put option

    // Calculate the Black-Scholes option price
    // The `bs` function in Commod.Options expects: f, k, v, t, payoff
    let callOptionPrice = Options.bs forwardPrice strikePrice volatility timeToExpiry optionType

    printfn "European Call Option Price: %f" callOptionPrice

    let putOptionPrice = Options.bs forwardPrice strikePrice volatility timeToExpiry Payoff.Put
    printfn "European Put Option Price: %f" putOptionPrice
    ```

## Running Tests

The library includes a suite of tests to ensure correctness and stability.

1.  **Navigate to the Test Directory**:
    Open your terminal or command prompt and change to the test project directory:
    ```bash
    cd src/Test
    ```

2.  **Run Tests**:
    Execute the tests using the .NET CLI:
    ```bash
    dotnet test
    ```
    This command will discover and run all automated tests defined in the `Test.fsproj` project.

    You might also find files like `ManualTests.fsx` within the test directory or examples. These typically require manual execution or specific setups and are used for more complex integration or scenario testing that isn't fully automated in the standard test suite.

## Documentation

For more in-depth explanations of the models, market conventions, data handling, advanced usage scenarios, and the overall architecture of CommodQuant, please refer to our detailed documentation:

*   **[Project Documentation on GitHub Pages](https://xqguo.github.io/CommodQuant/)**

The documentation site provides a comprehensive guide to help you get the most out of the library.

## Contributing

We welcome contributions from the community to help improve and expand CommodQuant! If you're interested in contributing, please follow these general steps:

1.  **Fork the Repository**: Create your own fork of the CommodQuant repository.
2.  **Create a Branch**: Make a new branch in your fork for your feature or bug fix (e.g., `feature/new-pricing-model` or `bugfix/issue-123`).
3.  **Make Changes**: Implement your changes, ensuring to adhere to the existing coding style. Please add relevant unit tests for any new functionality or bug fixes.
4.  **Test Your Changes**: Ensure all existing and newly added tests pass by running `dotnet test` in the `src/Test` directory.
5.  **Submit a Pull Request**: Push your changes to your fork and then submit a pull request to the main CommodQuant repository. Provide a clear and concise description of the changes you've made.

For major changes or new features, it's a good idea to open an issue first to discuss your proposed changes with the maintainers.

## License

This project is licensed under the MIT License - see the [LICENSE.txt](LICENSE.txt) file for details.
