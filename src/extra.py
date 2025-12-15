import requests
import pandas as pd

def get_gbif_occurrences(species_name, limit=300, max_records=200000):
    """
    Descarga todas las ocurrencias de una especie desde la API de GBIF.
    Pagina automáticamente hasta conseguir max_records o que no haya más datos.
    """
    offset = 0
    all_records = []

    print("Descargando ocurrencias desde GBIF...")

    while True:
        url = (
            "https://api.gbif.org/v1/occurrence/search?"
            f"scientificName={species_name}&"
            "hasCoordinate=true&"
            "limit=300&"
            f"offset={offset}"
        )

        r = requests.get(url)
        data = r.json()

        batch = data.get("results", [])
        all_records.extend(batch)

        print(f"Descargados: {len(all_records)} registros", end="\r")

        # detener si no hay más datos
        if len(batch) < limit:
            break

        # detener si excedemos lo deseado
        if len(all_records) >= max_records:
            print("\n⚠️ Se alcanzó max_records.")
            break

        offset += limit

    print(f"\nTotal descargado: {len(all_records)} registros")
    return pd.DataFrame(all_records)
