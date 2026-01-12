import pyarrow as pa

sources_schema = pa.schema([
    pa.field("id", pa.string()),
    pa.field("title", pa.string()),
    pa.field("source_type", pa.string()),
    pa.field("url", pa.string(), nullable=True),
    pa.field("abstract", pa.string(), nullable=True),
    pa.field("abstract_vector", pa.list_(pa.float32(), 1536), nullable=True),
    pa.field("full_text", pa.string(), nullable=True),
    # FIXME: LanceDB doesn't currently seem to support lists of lists well.
    # pa.field("full_text_vectors", pa.list_(pa.list_(pa.float32())), nullable=True),
])
