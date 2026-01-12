import pyarrow as pa

sources_schema = pa.schema([
    pa.field("id", pa.string()),
    pa.field("elfeed_feed_id", pa.string(), nullable=True),
    pa.field("elfeed_entry_id", pa.string(), nullable=True),
    pa.field("title", pa.string()),
    pa.field("source_type", pa.string()),
    pa.field("url", pa.string(), nullable=True),
    pa.field("abstract", pa.string(), nullable=True),
    pa.field("abstract_vector", pa.list_(pa.float32(), 1536), nullable=True),
    pa.field("full_text", pa.string(), nullable=True),
    # pa.field("full_text_vector", pa.list_(pa.float32(), 1536), nullable=True),
    pa.field("full_text_vectors", pa.list_(pa.list_(pa.float32(), 1536)), nullable=True),
])
