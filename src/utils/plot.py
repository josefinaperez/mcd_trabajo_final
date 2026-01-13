import geopandas as gpd
import matplotlib.pyplot as plt
import pandas as pd
from rasterio.mask import mask
import rasterio

def plot_points(shape_path, df=None, df_path=None):
    shape = gpd.read_file(shape_path)
    shape = shape.to_crs("EPSG:4326")
    
    if df is None and df_path is not None:
        df = pd.read_csv(df_path)
    
    gdf_points = gpd.GeoDataFrame(
        df,
        geometry=gpd.points_from_xy(df["decimalLongitude"], df["decimalLatitude"]),
        crs="EPSG:4326"
    )
    
    mask = gdf_points.within(shape.union_all())
    gdf_points_within = gdf_points[mask]
    
    ax = shape.plot(
        figsize=(6, 10),
        color="white",
        edgecolor="black"
    )

    gdf_points_within.plot(
        ax=ax,
        color="red",
        markersize=5
    )
    
    plt.axis("off")
    plt.show()


def plot_raster(shape_path, wc_path, title=""):

    shp = gpd.read_file(shape_path).to_crs("EPSG:4326")

    with rasterio.open(wc_path) as src:
        bio, transform = mask(
        src,
        shp.geometry,
        crop=True,
        filled=False
    )


    bio = bio[0]
    
    plt.figure(figsize=(3, 4))
    plt.imshow(bio)
    plt.colorbar(label=title)
    plt.title(title)
    plt.axis("off")
    plt.show()
