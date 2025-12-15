
KEEP_COLS = [
    'observed_on',
    'latitude',
    'longitude',
    'place_state_name',
    'place_country_name',
    'public_positional_accuracy'
]


def clean_dataset(df,
                  max_positional_accuracy,
                  keep_cols=KEEP_COLS):
    
    keep_cols = ['observed_on',
                 'latitude',
                 'longitude',
                 'place_state_name',
                 'place_country_name',
                 'public_positional_accuracy']
    
    df_clean = df[keep_cols].copy()
    df_clean = df_clean.dropna(subset=['latitude', 'longitude'])
    df_clean = df_clean[df_clean.public_positional_accuracy <= max_positional_accuracy]
    
    return df_clean