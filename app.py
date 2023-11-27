import json
from flask import Flask, render_template, request
from flask_cors import cross_origin
from llm_interaction import get_chat_completion


app = Flask(__name__)


@app.route('/')
def index():
    return render_template("index.html")


@app.route("/chat")
def chat():
    return render_template("chat.html")


@app.route("/api/chat", methods=["POST"])
@cross_origin(origins='*')
def converse():
    conversation = request.json
    next_message = get_chat_completion(conversation)
    return json.dumps({"msg": next_message})


if __name__ == '__main__':
    app.run()
