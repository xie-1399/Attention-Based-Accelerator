# vision transformer model with only Encoder
# the source code is from the https://github.com/jeonsworld/ViT-pytorch/blob/main/models/modeling.py
# difference is without resnet part
import math

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


# cal the attention scores seems is (matmul + trans + vec div + softmax)
class Attention(nn.Module): # get the q k v and attention scores
    def __init__(self,config,vis):
        super(Attention,self).__init__()
        self.vis = vis
        self.num_attention_heads = config.transformer["num_heads"] # 12
        self.attention_head_size = int(config.hidden_size / self.num_attention_heads) # 768 / 12 = 64
        self.all_head_size = self.attention_head_size * self.num_attention_heads # 768

        self.query = nn.Linear(config.hidden_size,self.all_head_size)
        self.key = nn.Linear(config.hidden_size,self.all_head_size)
        self.value = nn.Linear(config.hidden_size,self.all_head_size)

        self.out = nn.Linear(config.hidden_size, config.hidden_size)
        self.attn_dropout = nn.Dropout(config.transformer["attention_dropout_rate"])
        self.proj_dropout = nn.Dropout(config.transformer["attention_dropout_rate"])
        self.softmax = nn.Softmax(dim=-1)

    def transpose_for_scores(self,x):
        new_x_shape = x.size()[:-1] + (self.num_attention_heads,self.attention_head_size)
        x= x.view(*new_x_shape)
        return x.permute(0,2,1,3) # [B self.num_attention_heads H*w + 1 self.attention_head_size]

    def forward(self,hidden_states):
        mixed_query_layer = self.query(hidden_states)
        mixed_key_layer = self.key(hidden_states)
        mixed_value_layer = self.value(hidden_states)

        # reshape the data seems useful
        query_layer = self.transpose_for_scores(mixed_query_layer)
        key_layer = self.transpose_for_scores(mixed_key_layer)
        value_layer = self.transpose_for_scores(mixed_value_layer)

        # the vector div unit
        attention_scores = torch.matmul(query_layer,key_layer.transpose(-1,-2))
        attention_scores = attention_scores / math.sqrt(self.attention_head_size)
        attention_probs = self.softmax(attention_scores)
        attention_probs = self.attn_dropout(attention_probs)

        context_layer = torch.matmul(attention_probs, value_layer)
        context_layer = context_layer.permute(0, 2, 1, 3).contiguous()
        new_context_layer_shape = context_layer.size()[:-2] + (self.all_head_size,)
        context_layer = context_layer.view(*new_context_layer_shape)
        attention_output = self.out(context_layer)
        attention_output = self.proj_dropout(attention_output) # [N , H*W + 1,self.all_head_size]
        return attention_output


class MLP(nn.Module):
    def __init__(self,config):
        super(MLP,self).__init__()
        self.fc1 = nn.Linear(config.hidden_size,config.transformer["mlp_dim"])

class Block(nn.Module):
    pass


class Encoder(nn.Module):
    pass


if __name__ == '__main__':
    config = config.get_b16_config()
    embeddings = Embeddings(config,224,3)
    attn = Attention(config,False)
    x1 = embeddings(torch.randn(1,3,224,224))  # [N C H W] for the Conv2D
    x2 = attn(x1)
    print(x2)