import torch
from transformers import AutoTokenizer, AutoModel
from torch.utils.data import DataLoader
from functools import partial
import numpy as np
from tqdm import tqdm

def collate_fn(batch, tokenizer, max_length):
    """
    Collate function for the DataLoader that tokenizes a batch of texts.

    Parameters:
        batch (list[str]): List of texts.
        tokenizer (transformers.PreTrainedTokenizer): The tokenizer instance.
        max_length (int): Maximum sequence length.

    Returns:
        dict: Tokenized batch with keys such as 'input_ids' and 'attention_mask'.
    """
    return tokenizer(
        batch,
        max_length=max_length,
        truncation=True,
        padding=True,
        return_tensors="pt"
    )

def mean_pooling(model_output, attention_mask):
    """
    Computes mean pooling over token embeddings while ignoring padding tokens.

    Parameters:
        model_output (transformers.modeling_outputs.BaseModelOutputWithPoolingAndCrossAttentions):
            The output from the transformer model.
        attention_mask (torch.Tensor): Attention mask indicating real tokens vs padding.

    Returns:
        torch.Tensor: Mean-pooled sentence embeddings.
    """
    token_embeddings = model_output.last_hidden_state  # (batch_size, seq_len, hidden_dim)
    input_mask_expanded = attention_mask.unsqueeze(-1).expand(token_embeddings.size())
    sum_mask = input_mask_expanded.sum(dim=1).clamp(min=1e-9)
    return (token_embeddings * input_mask_expanded).sum(dim=1) / sum_mask

def get_text_embeddings(texts, 
                        model_name="distilbert/distilbert-base-multilingual-cased", 
                        batch_size=32, 
                        max_length=512, 
                        use_fp16=False):
    """
    Generates embeddings for a list of texts using the specified transformer model.

    This function loads the tokenizer and model, tokenizes the texts in batches,
    processes them on GPU (if available), and returns the final embeddings as a NumPy array.

    Parameters:
        texts (list[str]): List of texts to encode.
        model_name (str): Hugging Face model name.
        batch_size (int): Batch size for processing.
        max_length (int): Maximum sequence length for tokenization.
        use_fp16 (bool): Whether to use fp16 precision on CUDA devices (default is False).

    Returns:
        np.ndarray: The sentence/document embeddings.
    """
    # Select device: GPU if available, otherwise CPU.
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    
    # Load tokenizer and model (using fast tokenizer for efficiency)
    tokenizer = AutoTokenizer.from_pretrained(model_name, use_fast=True)
    model = AutoModel.from_pretrained(model_name).to(device).eval()
    
    # Optionally set the model to fp16 precision if using a CUDA device and requested.
    if device.type == "cuda" and use_fp16:
        model = model.half()
    
    # Create a picklable collate function using functools.partial
    collate = partial(collate_fn, tokenizer=tokenizer, max_length=max_length)
    
    # Create DataLoader; for reticulate compatibility, use num_workers=0
    dataloader = DataLoader(
        texts,
        batch_size=batch_size,
        collate_fn=collate,
        num_workers=0,  # Use 0 when calling from reticulate to avoid multiprocessing issues.
        pin_memory=True
    )
    
    embeddings = []
    
    # Disable gradient computation for inference
    with torch.no_grad():
        for batch in tqdm(dataloader, desc="Processing Batches", leave=False):
            # Move the batch to the selected device
            batch = {key: val.to(device) for key, val in batch.items()}
            outputs = model(**batch)
            # Compute masked mean pooling for sentence embeddings
            batch_embeddings = mean_pooling(outputs, batch["attention_mask"]).cpu()
            embeddings.append(batch_embeddings)
    
    # Concatenate all batch embeddings and convert to a NumPy array
    return torch.cat(embeddings, dim=0).numpy()
