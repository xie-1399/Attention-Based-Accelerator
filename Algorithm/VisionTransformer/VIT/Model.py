# vision transformer model with only Encoder
# the source code is from the https://github.com/jeonsworld/ViT-pytorch/blob/main/models/modeling.py
# difference is without resnet part

import torch
import torch.nn as nn
import numpy as np
from torch.nn.modules.utils import _pair
import config
# make the img to patches and embedding the patches

# let the embeddings on the CPU or GPU to get the value
class Embeddings(nn.Module):
    def __init__(self,config,img_size,in_channels = 3):
        super(Embeddings,self).__init__()
        img_size = _pair(img_size) # repeat 2 times
        patch_size = _pair(config.patches["size"])
        n_patches = (img_size[0] // patch_size[0]) * (img_size[1] // patch_size[1]) # 14 * 14
        self.patch_embeddings = nn.Conv2d( # x is [N C H W]
            in_channels=in_channels,
            out_channels=config.hidden_size,
            kernel_size=patch_size,
            stride=patch_size
        )
        self.position_embeddings = nn.Parameter(torch.zeros(1, n_patches+1, config.hidden_size))
        self.cls_token = nn.Parameter(torch.zeros(1, 1, config.hidden_size))
        self.dropout = nn.Dropout(config.transformer["dropout_rate"])

    def forward(self,x):
        B = x.shape[0]
        cls_tokens = self.cls_token.expand(B, -1, -1) # [N,1,hidden_size]

        x = self.patch_embeddings(x) # [N,hidden_size,H,W]
        x = x.flatten(2) # [N,hidden_size,H*W]
        x = x.transpose(-1, -2) # [N,H*W,hidden_size]
        x = torch.cat((cls_tokens, x), dim=1)

        embeddings = x + self.position_embeddings
        embeddings = self.dropout(embeddings) # scale the value and then drop some out
        return embeddings # [N, H*W+1 , hidden_size]


class MLP(nn.Module):
    def __init__(self,config):
        super(MLP,self).__init__()
        self.fc1 = nn.Linear(config.hidden_size,config.transformer["mlp_dim"])


class Attention(nn.Module):
    pass

class Block(nn.Module):
    pass


class Encoder(nn.Module):
    pass


if __name__ == '__main__':
    config = config.get_b16_config()
    embeddings = Embeddings(config,224,3)
    print(embeddings(torch.randn(1,3,224,224)))  # [N C H W] for the Conv2D