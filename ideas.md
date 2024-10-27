- more proxies
- sim_field: custom raster input; custom number of covariates; custom selection of proxies
- remove duplicated points (maybe rewrite of the sampling function is needed??)

Steps:
1. Simulate a field with specified parameters
2. Add proxies to the field
3. Sample inter and extra sample data (maybe improve sampling by taking polygons, or automatically detect these areas?)
4. Create a modeling wrapper/s
5. Write models' summarizer


Question:
- How to simulate a field with changing spatial patterns?