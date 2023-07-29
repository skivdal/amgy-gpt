"use strict";

document.addEventListener("DOMContentLoaded", () => {
    const conversation = [];

    const messageBox = document.getElementById("chat-message");
    const sendMessageBtn = document.getElementById("send-message");
    const loader = document.getElementById("message-loader");

    sendMessageBtn.addEventListener("click", async () => {
        conversation.push(messageBox.value);
        messageBox.value = "";

        sendMessageBtn.disabled = true;
        loader.style.display = "inline";

        renderConversation(conversation);

        try {
            const req = await fetch("/api/chat", {
                method: "POST",
                body: JSON.stringify(conversation),
                headers: {
                    "Content-Type": "application/json",
                },
            });

            const res = await req.json();

            conversation.push(res.msg);
        } catch {
            conversation.push('<span style="color: red;">There was an error in processing this request.</span>');
        }

        sendMessageBtn.disabled = false;
        loader.style.display = "none";

        renderConversation(conversation);
    });
});

function renderConversation(conversation) {
    const element = document.getElementById("conversation");
    element.innerHTML = "";

    for (let i = 0; i < conversation.length; ++i) {
        const msg = conversation[i];

        const r = document.createElement("tr");
        const d = document.createElement("td");
        const m = document.createElement("td");

        d.style.textAlign = "right";
        d.style.verticalAlign = "top";
        d.innerHTML = i % 2 === 0 ? "You:" : "amgyGPT:";

        m.innerHTML = msg.replaceAll("\n", "<br>");

        r.appendChild(d);
        r.appendChild(m);

        element.appendChild(r);
    }
}
