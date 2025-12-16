import numpy as np
import rasterio


def sample_raster_at_points(
    df,
    raster_path,
    lat_col="latitude",
    lon_col="longitude",
    value_col="raster_value",
    nodata_value=np.nan,
):
    """
    Sample raster values at point locations.

    Parameters
    ----------
    df : pandas.DataFrame
        DataFrame containing point coordinates.
    raster_path : str or Path
        Path to raster file.
    lat_col : str
        Name of latitude column.
    lon_col : str
        Name of longitude column.
    value_col : str
        Name of output column.
    nodata_value : float
        Value to assign when raster has no data.

    Returns
    -------
    pandas.DataFrame
        Copy of df with new column containing raster values.
    """
    coords = list(zip(df[lon_col], df[lat_col]))

    with rasterio.open(raster_path) as src:
        values = [
            val[0] if val is not None else nodata_value
            for val in src.sample(coords)
        ]

    df_out = df.copy()
    df_out[value_col] = values

    return df_out
