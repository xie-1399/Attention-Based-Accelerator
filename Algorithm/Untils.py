import torch
from torch import nn

'''
really simple network for testing the env running well 
'''

device = "cuda" if torch.cuda.is_available() else "cpu"
print("Using {} device".format(device))

class NeuralNetwork(nn.Module):
    def __init__(self):
        super(NeuralNetwork,self).__init__()
        self.flatten = nn.Flatten()
        self.linear_relu_stack = nn.Sequential(
            nn.Linear(28 * 28,512),
            nn.ReLU(),
            nn.Linear(512,512),
            nn.ReLU(),
            nn.Linear(512,10),
            nn.Softmax()
        )
    def forward(self,x):
        x = self.flatten(x)
        logits = self.linear_relu_stack(x)
        return logits

def feedNeuralNetwork(data,model):
    model.eval() # not using the batchNorm and the dropOut
    data.to(device)
    with torch.no_grad():
        pred = model(data)
        print("NeuralNetwork forward result is : {pred}".format(pred = pred))
feedNeuralNetwork(torch.rand((3,28 * 28)),NeuralNetwork())


'''
the really lib torch usage / RTFM is the best way
'''
ones = torch.ones(4,4)
sums = ones.sum().item() # convert to the python types