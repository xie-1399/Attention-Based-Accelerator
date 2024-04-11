'''
the vilt model in the transformer lib usage
'''


import torch
from transformers import ViltModel,ViltConfig,ViltProcessor,ViltForQuestionAnswering
from PIL import Image
import requests
from huggingface_hub import snapshot_download

configuration = ViltConfig()
model = ViltModel(configuration)

# print(model) # 12 vilt layers encoder

modelPath = "./VQA"

def download(repo_id):
    snapshot_download(repo_id=repo_id)


def ViltModelUsage():
    from transformers import ViltProcessor, ViltForQuestionAnswering
    import requests
    from PIL import Image

    # prepare image + question
    url = "http://images.cocodataset.org/val2017/000000039769.jpg"
    image = Image.open(requests.get(url, stream=True).raw)
    text = "How many cats are there?"

    processor = ViltProcessor.from_pretrained(modelPath)
    model = ViltForQuestionAnswering.from_pretrained(modelPath)

    # prepare inputs
    encoding = processor(image, text, return_tensors="pt")

    # forward pass
    outputs = model(**encoding)
    logits = outputs.logits
    idx = logits.argmax(-1).item()
    print("Predicted answer:", model.config.id2label[idx])

# test the VQA request and get the answer
if __name__ == '__main__':
    ViltModelUsage()