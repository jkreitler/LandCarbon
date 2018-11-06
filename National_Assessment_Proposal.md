USGS LandCarbon Sustained Assessment
================

**Principal Investigators:** Benjamin M. Sleeter (Research Geographer,
Western Geographic Science Center); Todd J. Hawbaker (Research
Geographer); Camille Stagg (Research Ecologist, National Wetland Science
Center)

### Research Proposal Overview

We propose a 3-year research project aimed at developing a
state-of-the-art capability to assess carbon storage and flux in
ecosystems of the United States. This project will build on the
experience and lesons-learned from the first USGS Biological Carbon
Sequestration Assessment and strive to make a number of scientific and
methodological improvements. At its core, this new assessment activity
will leverge a number of new and innovative research capabilities
developed since completion of the first assessment, including
integration within the USGS’s Land Use and Carbon Scenario Simulator
(LUCAS) modeling framework.

### Research Approach

The 3-year project will consist of three priamry stages and will begin
in earnest in the Winter/Spring of 2019. Culmination of this project
will be achieved with publication and the release of an updated
assessment in September 2021.

1.  **Planning and methodological development:** Similar to the original
    assessment, we will develop and outline a methodological approach
    aimed at developing a sustained LandCarbon assessment capability.
    This will be achieved by gathering relative experts in the fields of
    land change, ecosystem carbon processes, climate change, and natural
    disturbances for a series of focused workshops aimed at
    1)  identifying the major controlling processes to be considered,
    2)  prioritizing gaps in current knowledge for inclusion into the
        assessment,
    3)  defining the boundaries of the assessment,
    4)  laying out a structured approach to produce repeatable and
        reprodibible results, and
    5)  developing a tiered system for updating future assessments where
        current knowledge gaps are prioritized for inclusion in
        subsequent versions.
2.  **Data collection, model development and implementation:** This
    phase of the research will begin in ernest in the Summer of 2019 and
    extend throughout the duration of the project. Given the scale and
    scope of the proposed project, we propose to minimize new data
    generation where possible by leverging current publically available
    resources whenever possible. For example, existing downscaled
    climate data will be utilized, as opposed to generating new
    datasets. Similarily, scenario projections will be based on the
    latest publically available databases consistent with IPCC research
    activites. Where no current data exists (e.g. climate-driven
    projections of disturbances) we will leverage approaches described
    in literature. Data collection activities generally include the
    following categories:
    1)  Future scenario framework (RCPs, SSP, global mitigation
        scenarios),
    2)  Historical and future projected changes in weather and climate
        (PRISM, LOCA),
    3)  Historical and projected rates of land use, land-use change,
        land management (LC Trends, NLCD, LCMAP),
    4)  Natural disturbance regimes and their projected changes due to
        climate and land use (MTBS, Landfire, Burn Area ECV),
    5)  Initial conditions, including current land-use/land-cover and
        vegetation composition (Landfire, NLCD, LCMAP).
3.  **Development of standardized reporting and publication:** Key to
    success of this project will be the development of a framework for
    publication of results. Unlike previous efforts, we will develop a
    standardized reporting system which is integrated within the actual
    modeling environment. The goal of this activity is two-fold. First,
    the integration of a standardized set of reports will reduce the
    “time-to-delivery” once all model runs and analysis has been
    finished. Second, this effort will provide an open and transparent
    reporting framework which will facilitate the peer review process.
    As such, activites related to reporting and publication, including
    development of online data access will be run in parallel with other
    scientific activies described in the previous section. To achieve
    this goal the project team will work with USGS experts in
    publications and online visualization to ensure assessment products
    can be provided in a timely manor.

### Modeling Approach

We propose to use the USGS LUCAS modeling framework to conduct the
assessment (*1*).

# Project Overview

**Structure of assessment**

  - Purpose, justification, and context for the assessment (Zhu and
    Reed)
      - Purpose/goals
      - Justification
          - EISA 2007 requirements/interpretation
      - Scope
          - What is included and what isn’t?
          - Carbon pools and fluxes considered
              - Live, DOM, Soil, HWP?
          - Thematic resolution
              - Forest classification (Can we link to LANDFIRE EVT or
                some coarser classification?)
          - Spatial extent – national? CONUS vs including AK and HI.
          - Spatial and temporal resolution
          - Reporting units
  - Context – How does the LandCarbon assessment relate to other work?
      - IPCC
      - EPA GHG inventory
      - FS FIA inventory
      - State of the Carbon Cycle Report
      - National Climate Change Assessment
      - LandCarbon v1
  - Modeling Approach
      - LUCAS model as a modeling framework.
          - External models either linked or coupled.
          - Minimize amount of work that can not be connected (in code).
          - Statistical

**Main findings (Everyone)**

  - Baseline carbon stocks and fluxes
      - Nationally, and by ecoregion and state
      - Public vs. private lands
      - How have they changed over time?
      - Attribution - In response to what drivers?
      - Uncertainties
      - How do they compare to fossil fuel emissions?
  - Projected change under business as usual scenario with climate
    change projections
      - Where are carbon stocks most vulnerable?
      - Mitigation scenarios to enhance biological carbon sequestration
      - Where can we protect existing stocks and enhance future stocks?
  - Integrated modeling approach (Sleeter)
      - focus on integration of carbon dynamics with land/water surface
        dynamics?
      - input and output uncertainties
      - attribution
  - Ecosystem carbon dynamics
      - Terrestrial (Selmants and Liu)
      - Can we simulate stock changes using FIA and FVS by EVT for
        disturbances/harvest/ecoregion? Can we get LANDFIRE to do this
        for us?
      - Wetland (Stagg and R. Sleeter)
      - Aquatic
      - Agriculture
      - Urban/developed (Diffendorfer?)
      - Where to address lateral fluxes?
  - Land/water surface dynamics
      - Baseline land-use patterns and future land-use scenarios
        (Sleeter)
      - Baseline disturbance and future potential disturbances (Hawbaker
        and Henne), Wildfire fire, Prescribed fire (with Tall Timbers)
  - Vegetation feedbacks with climate and disturbance (Henne and
    Vanderhoof)
      - Succession
      - Type conversion
  - Climate change scenarios and impacts
      - Use Abatzglou’s data for CONUS -
        <http://www.climatologylab.org/products.html>
      - Mitigation actions, impacts, and scenarios
      - Case studies vs large scale?
      - Fuel treatments (Krietler and Hawbaker)
      - Land use?
      - Forestry – primarily harvesting patterns and methods
      - Rangelands
      - Wetlands (R. Sleeter?)
      - Agriculture?
      - DOI lands
      - Economics of mitigation actions (Bagstad and Kreitler and
        Pindilli)?
  - Data delivery and visualization (Sherba)
      - Input and output data
      - Models/source code (if public) or executables
      - Visualization tools
  - Planned topics for future assessments
      - Disturbances
      - Insects v3 (with FS FHAAST)
      - Drought v3 or v4 (Henne and Craig Allen’s team)
  - Critical infrastructure:
      - Computing power and shared storage?  
      - Will YETI2 be online at EROS?
      - Cloud services?
          - Working archive for data, code, and models?
  - Critical data:
      - MTBS and BAECV?
          - Surface water extent ECV?
      - LANDFIRE veg, fuels, and disturbances (especially fuel
        treatments)?
      - NLCD/LCMAP?
      - Better wetland maps?
      - Climate data - baseline and projected
  - Partners:
      - EPA?
      - LANDFIRE fuels team if going the FIA-FVS route
      - FIA (Forest Health Assessment and Applied Science Team (FHAAST)
        for Insect disturbances)

# Project Staff

  - Headquarters
      - Zhiliang Zhu, Project Chief Biological Carbon Sequestration
        Project; <zzhu@usgs.gov>
      - Bradley Reed, Program Coordinator Land Change Science,
        <reed@usgs.gov>
  - Western Geographic Science Center (WGSC)
      - Benjamin M. Sleeter, Research Geographer; <bsleeter@usgs.gov>
      - Jinxun Liu, Research Physical Scientist; <jxliu@usgs.gov>
      - Jason Kreitler; Research Geographer; <jkreitler@usgs.gov>
      - Paul Selmants; Research Physical Scientist; <pselmants@usgs.gov>
      - Jason Sherba; Geographer; <jsherba@usgs.gov>
  - Geosciences and Environmental Change Science Center (GECSC)
      - Todd J. Hawbaker
      - Paul Henne
  - Wetland and Aquatic Research Center (WARC)
      - Camille Stagg,
      - Brady Coulvillion
  - Hydrologic Remote Sensing Branch (HRSB)
      - Rachel Sleeter

# Goals & Objectives

The goal of this research activity is to a) develop a standardized
methodological approach to assess changes in carbon storage and flux in
ecosystems of the United States, and b) implement the methodology and
produce and annual series of reports describing changes in ecosystem
carbon balance and the relative effects of major controlling processes.

  - Goal 1: Develop a repeatable and transparant methodology to assess
    ecosystem carbon stocks and fluxes for ecosystems of the United
    States.
      - Objective \#1:
      - Objective \#2:
  - Goal 2: Describe recent historical changes in ecosystem carbon
    balance and the major controlling processes.
      - Objective \#1:
      - Objective \#2:
  - Goal 3: Develop alternative future projections based on alternative
    scenarios.
      - Objective \#1:
      - Objective \#2:

# Scope

### Temporal Domain

Them temporal domain covers the recent historical past (Landsat epoch)
as well as projections into the future.

### Spatial Domain

The scope of this research activity spans terrestrial and aquatic
ecosystems of the United States.

# Phase 1

# Deliverables

# Budget

# References

<div id="refs" class="references">

<div id="ref-sleeter2018effects">

1\. B. M. Sleeter *et al.*, Effects of contemporary land-use and
land-cover change on the carbon balance of terrestrial ecosystems in the
united states. *Environmental Research Letters*. **13**, 045006 (2018).

</div>

</div>