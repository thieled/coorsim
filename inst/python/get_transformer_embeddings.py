import torch
from transformers import AutoTokenizer, AutoModel
from torch.utils.data import Dataset, DataLoader
import numpy as np
from tqdm import tqdm

class TextDataset(Dataset):
    def __init__(self, texts, tokenizer, max_length=512):
        self.texts = texts
        self.tokenizer = tokenizer
        self.max_length = max_length

    def __len__(self):
        return len(self.texts)

    def __getitem__(self, idx):
        # Use the tokenizer's __call__ method directly
        tokens = self.tokenizer(
            self.texts[idx],
            max_length=self.max_length,
            truncation=True,
            padding='max_length',
            return_tensors="pt"
        )
        return {key: val.squeeze(0) for key, val in tokens.items()}

def get_text_embeddings(texts, model_name='twitter/twhin-bert-base', batch_size=32):
    tokenizer = AutoTokenizer.from_pretrained(model_name, use_fast=True)
    model = AutoModel.from_pretrained(model_name)

    # Move model to GPU
    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    model.to(device)

    dataset = TextDataset(texts, tokenizer)
    dataloader = DataLoader(dataset, batch_size=batch_size, collate_fn=lambda x: tokenizer.pad(x, return_tensors="pt"))

    embeddings = []

    # Process the dataset in batches
    with torch.no_grad():
        for batch in tqdm(dataloader, desc="Processing Batches"):
            # Move batch to the device
            batch = {key: val.to(device) for key, val in batch.items()}
            outputs = model(**batch)
            
            # Mean pooling for sentence embeddings
            batch_embeddings = outputs.last_hidden_state.mean(dim=1).cpu().numpy()
            embeddings.append(batch_embeddings)

    return np.vstack(embeddings)
