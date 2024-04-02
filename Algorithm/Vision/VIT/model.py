'''
the model is the transformer encoder
    Author: xie-1399
    Date:  2024/4/2
'''

import torch
from torch import nn

class PositionalEmbedding1D(nn.Module):
    def __init__(self,seq_len,dim):
        super.__init__()
        self.pos_embedding = nn.Parameter(torch.zeros(1,seq_len,dim))

    def forward(self,x):
        pass


class MultiHeadSelfAttention(nn.Module):
    '''Multi-Head Dot Product Attention '''
    def __init__(self,dim,num_heads,dropout):
        super().__init__()
        self.proj_q = nn.Linear(dim,dim)
        self.proj_k = nn.Linear(dim,dim)
        self.proj_v = nn.Linear(dim,dim)
