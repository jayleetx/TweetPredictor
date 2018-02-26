from twarc import Twarc
import pprint
import json

consumer_key = "2NBPNFml9TtV3ValyhgZqP4ch"
consumer_secret = "qzCNGbr5I5vD2GAps7gdsQRNW4GbmlhODp0BokqFgCzLw2TjjV"
access_token = "931008641255084032-rMD6zn8esls7S1z4UiebC52Tb0gp8BM"
access_token_secret = "kpxBObeQfcpqbU8EikrionXFa1NbYpstYwPGA542av7K3"


output = open("sample1.json", 'w')
t = Twarc(consumer_key, consumer_secret, access_token, access_token_secret)
hydrated = []

count = 0
for tweet in t.hydrate(open('representatives.txt')):
    if count > 10000:
        break

    count += 1
    hydrated.append(tweet)

    if count == (0 % 1000):
        output.write(json.dump(hydrated))


print("done!")
print(count, " tweets pulled.")
output.write(json.dumps(hydrated))



