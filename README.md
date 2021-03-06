<div id="devex-badge"><a rel="Exploration" href="https://github.com/BCDevExchange/assets/blob/master/README.md"><img alt="Being designed and built, but in the lab. May change, disappear, or be buggy." style="border-width:0" src="https://assets.bcdevexchange.org/images/badges/exploration.svg" title="Being designed and built, but in the lab. May change, disappear, or be buggy." /></a></div>

# Grizzly Bear IUCN Threat Assessment

This repository contains [R](https://www.r-project.org/) code that summarizes spatial & tabular data to assess Grizzly Bear&mdash;by [Grizzly Bear Population Unit](https://catalogue.data.gov.bc.ca/dataset/caa22f7a-87df-4f31-89e0-d5295ec5c725)&mdash;using the [IUCN threat classification scheme](http://www.iucnredlist.org/technical-documents/classification-schemes/threats-classification-scheme).

## Data
**Land Cover Data**:
This analysis uses the British Columbia [Baseline Thematic Mapping present land use data BTM file](https://catalogue.data.gov.bc.ca/dataset/baseline-thematic-mapping-present-land-use-version-1-spatial-layer) distributed under the [Access Only - B.C. Crown Copyright](https://www2.gov.bc.ca/gov/content?id=1AAACC9C65754E4D89A118B875E0FBDA) licence.
  
**Road Data**:
This analysis uses the British Columbia [Digital Road Atlas available from the B.C. Data Catalogue](https://catalogue.data.gov.bc.ca/dataset/bb060417-b6e6-4548-b837-f9060d94743e) and distributed under the [Access Only - B.C. Crown Copyright](https://www2.gov.bc.ca/gov/content?id=1AAACC9C65754E4D89A118B875E0FBDA) licence. The Digital Road Atlas is the [best available single source of road data for the Province of B.C.](https://www2.gov.bc.ca/gov/content?id=21FFEC94B0AD40818D2D2AF06D522714) Metadata details for the Digital Road Atlas (DRA) are available in PDF format from the [B.C. Data Catalogue](https://catalogue.data.gov.bc.ca/dataset/bb060417-b6e6-4548-b837-f9060d94743e).

The road analysis excludes some surface and road types in the [Digital Road Atlas](https://catalogue.data.gov.bc.ca/dataset/bb060417-b6e6-4548-b837-f9060d94743e). Boat (B), overgrown (O) & decomissioned (D) roads are excluded from `TRANSPORT_LINE_SURFACE_CODE` and ferry routes (F, FP, FR, RWA), non-motorized trails (T, TD), road proposed (RP), and road pedestrian mall (RPM) are excluded from `TRANSPORT_LINE_TYPE_CODE`.

The road analysis is based on [rasterized input data](https://en.wikipedia.org/wiki/Raster_data), generated with [R](https://www.r-project.org/) code that is also available in [GitHub](https://github.com/bcgov/bc-raster-roads).

**Cumulative Effects Grizzly Bear Current Condition Data**:
This analysis uses data developed by British Columbia to evaluate indicators of Grizzly Bear current condition. Data available upon request.

## Usage

There are four core scripts that are required for the IUCN threat assessment analysis, they need to be run in order:

-   01\_load.R
-   02\_clean.R
-   03\_analysis.R
-   04\_output.R

Or you can run all four scripts using run_all.R.

All packages used in the analyses can be installed from CRAN using `install.packages()`.

## Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov-c/intact-landcover/issues/).

## How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.

## Licence

    Copyright 2018 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.


This repository is maintained by [ENVEcosystems](https://github.com/orgs/bcgov/teams/envecosystems/members).