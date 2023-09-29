# Introduction 
This folder contains files that can be used to estimate economic impacts of spending in the UK or Scottish economies.
The economic metrics calculated area: Output, GVA, and Jobs

# Summary
Two models have been developed:
1) UK IO model, which calculates IO multipliers using UK input-output tables. 
Multiple sources of UK IO tables exist. The file "uk_model.R" currently uses the ONS UK IO table from 2019.

2) Scottish IO model, which calculates IO multipliers using Scottish input-output tables.
The file "scotland_model.R" uses the Scottish Government's Input-output table for 2019. 

Both models require additional files to calculate economic impacts. 

First a mapping file is required to map the different industries of the IO tables (as these are usually constructed at different aggregations).
Each model file is already constructed so industries are aggregated automatically. 
To do this, you first need to choose the aggregation of your industries, and how this maps to the original SIC classifications.
You then need to tell the model this mapping in the "Mappings.xlsx" file. Input your new industries in the "New Industry" column for each tab.
(you can ignore the OECD_Mapping and Regional_GVA_Mapping tabs as these are not currently used).

Next, you need to tell the model how much will be spent in the economy from your proposed business activity.
The "Spending_Vector.xlsx" file is the core file, which describes the increase in spending across each industry of the economy resulting from your new business activity.
For example, if you want to estimate the potential impacts of a project that spends £Xmill across the economy, input how this spreads across agriculture, food products, financial activities etc.
The individual spending in each industry must be inputted into the "Spending_Vector.xlsx" file.

The IO models estimate three types of economic impacts:
1) Direct Effect: this is the direct increase in output from spending in one particular industry (i.e. spending £Xmill in agriculture increases output in agriculture by £Xmill) 
2) Indirect Effect: this is the increase in spending due to increased demand for suppliers (i.e. spending £Xmill in agriculture generates £Ymill spending on transport due to supply chains)
3) Induced Effect: this is the increase in spending in the economy due to higher household income (i.e. spending £Xmill in agriculture results in £Zmill more being paid to households, which results in further spending in the economy). 

IO models calculate 2 types of multipliers:
1) Type I multipliers sum together direct and indirect effects 
2) Type II multipliers are Type 1 multipliers plus induced effects.

IO models typically deal with output effects. This means that they are calculating the £Ymill increase in output due to a £Xmill increase in spending.
All output effects are converted into GVA and employment effects using ratios. 

GVA-Output ratios are derived from the Input-Output tables. GVA-Jobs ratios are calculated using BRES data.
"UK_BRES_2019_Employment.xlsx" contains information on UK employment estimates. This is used to calculate UK specific GVA-Jobs ratios.
"Scotland_BRES_2019_Employment.xlsx" contains information on Scottish employment estimates. This is used to calculate Scotland specific GVA-Jobs ratios.

It is fine to calculate Scottish economic impacts using either the UK or Scottish IO Models (although it could be argued the latter is more accurate).
However, if there is spending outside of Scotland, the UK IO model should be used.

"Scotland IO vs UK IO model.png" is a snip of a comparison between the two models. Using the same shock,
the UK model estimates higher indirect and induced effects. This is due to taking into account more integrated supply chains across the UK economy.
For example, spending £Xmill in the Scottish economy will have indirect and induced impacts from the English, Welsh and Northern Irish economies.
The UK model takes into account these interdependencies, including second, third etc order effects.
The Scotland model does not take into second, third etc order effects across the whole of the UK.

"Economic Modelling & Regional Analysis Note.docx" discusses IO and CGE modelling frameworks, and the possibility to regionalise economic effects.

 