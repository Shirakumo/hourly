class Hourly{
    constructor(){
        var self = this;
        self.cache = {};
        self.loading = {};
        if(console.log === undefined)
            self.log = ()=>{};
        else
            self.log = function(){
                var args = Array.prototype.slice.call(arguments);
                args.unshift("[Courier]");
                return console.log.apply(console, args);
            };

        self.log("Init");

        self.apiRoot = document.querySelector("head link[rel=api-root]").getAttribute("href");
        if(!self.apiRoot){
            self.log("Failed to retrieve API root. WTF?");
        }

        var url = [location.protocol, '//', location.host, location.pathname].join('');
        self.registerElements();
    }

    registerElements(element){
        element = element || document;
        var self = this;
        self.registerAll(element, ".chart", self.registerChart);
        self.registerAll(element, "form", self.registerForm);
        self.registerAll(element, "input[type=datetime-local]", self.registerDateInput);
        self.registerAll(element, "input[type=time]", self.registerTimeInput);
    }

    registerAll(element, query, regger){
        var self = this;
        var elements = element.querySelectorAll(query);
        for(var i=0; i<elements.length; ++i)
            regger.apply(self, [elements[i]]);
    }

    registerDateInput(element){
        if(element.value == ""){
            var date = new Date(),
                ten = function (i) {
                    return (i < 10 ? '0' : '') + i;
                },
                YYYY = date.getFullYear(),
                MM = ten(date.getMonth() + 1),
                DD = ten(date.getDate()),
                HH = ten(date.getHours()),
                II = ten(date.getMinutes());
            element.value = YYYY+'-'+MM+'-'+DD+'T'+HH+':'+II;
        }
    }

    registerTimeInput(element){
        if(element.value == ""){
            element.valueAsNumber = 0;
        }
    }

    registerForm(element){
        var self = this;
        var save = element.querySelector("input[type=submit]");
        if(!save || save.dataset.nofetch) return;
        if(element.classList.contains("search")) return;
        save.addEventListener("click", (ev)=>{
            var target = save.getAttribute("formaction") || element.getAttribute("action");
            ev.preventDefault();
            if(!self.loading[target]){
                if(element.checkValidity()){
                    self.showSpinner();
                    self.loading[target] =
                        self.apiCall(target, element)
                        .then((r)=>{window.location.replace(r.target);},
                              (r)=>{self.showError(r.message ||
                                                   new DOMParser().parseFromString(r, "text/html").querySelector("title").innerText);})
                        .finally(()=>{delete self.loading[target];
                                      self.showSpinner();});
                }else{
                    element.reportValidity();
                }
            }
            return false;
        });
    }

    constructElement(tag, options){
        var self = this;
        var el = document.createElement(options.tag || tag);
        (options.classes||[]).forEach(function(clas){
            if(clas) el.classList.add(clas);
        });
        if(options.text) el.innerText = options.text;
        if(options.html) el.innerHTML = options.html;
        for(var attr in (options.attributes||{})){
            if(options.attributes[attr])
                el.setAttribute(attr, options.attributes[attr]);
        }
        (options.elements||[]).forEach(function(element){
            el.appendChild(self.constructElement(element.tag, element));
        });
        return el;
    }

    showPopup(content){
        var self = this;
        var popup = self.constructElement("div",{
            classes: ["popup"],
            elements: [{tag:"div", classes:["content"]}]
        });
        popup.hide = ()=>{
            popup.parentNode.removeChild(popup);
        };
        popup.querySelector(".content").appendChild(content);
        popup.addEventListener("click", (ev)=>{
            if(ev.target == popup)popup.hide();
        });
        document.querySelector("body").appendChild(popup);
        return popup;
    }

    showError(content){
        var box = document.querySelector(".box.error");
        var updated = box.cloneNode(true);
        updated.innerText = content;
        box.parentNode.replaceChild(updated, box);
    }

    showSpinner(options){
        var self = this;
        options = options || {};
        var spinner = document.querySelector(".spinner");
        if(options.activate === undefined){
            options.activate = (spinner)? false : true;
        }
        if(options.activate && !spinner){
            spinner = self.constructElement("div", {
                classes: ["popup", "spinner", options.classes],
                elements: [
                    {
                        tag: "div",
                        text: options.message || "Please Wait",
                        classes: ["container"],
                        elements: [{tag:"div"}, {tag:"div"}]
                    }
                ]
            });
            document.querySelector("body").appendChild(spinner);
        }else if(spinner){
            spinner.parentElement.removeChild(spinner);
        }
        return spinner;
    }

    apiCall(endpoint, args, methodArgs){
        var self = this;
        methodArgs = methodArgs || {};
        methodArgs.format = methodArgs.format || "json";
        return new Promise((ok, fail)=>{
            var request = new XMLHttpRequest();
            var formData;

            if(!(endpoint.startsWith("http://") || endpoint.startsWith("https://"))){
                endpoint = self.apiRoot+endpoint;
            }

            if(args instanceof HTMLElement){
                formData = new FormData(args);
                formData.delete("browser");
            }else if(args instanceof FormData){
                formData = args;
            }else{
                formData = new FormData();
                for(var field in args){
                    formData.append(field, args[field]);
                }
            }

            if(methodArgs.format == "json")
                formData.append("data-format", "json");
            request.onload = ()=>{
                var data = request.responseText;
                var status = request.status;
                if(request.getResponseHeader("Content-Type").includes("application/json")){
                    data = JSON.parse(data);
                    status = data.status || status;
                }
                if(status === 200){
                    self.log("Request succeeded", data);
                    ok(data);
                }else{
                    self.log("Request failed", data);
                    fail(data);
                }
            };
            self.log("Sending request to",endpoint);
            request.open("POST", endpoint);
            request.send(formData);
        });
    }

    registerChart(element){
        var self = this;
        var ctx = element.querySelector("canvas").getContext("2d");
        var chart = null;
        var refresh = ()=>
            self.apiCall(element.getAttribute("action"),element)
            .then((r)=>{
                chart.data.labels = r.data.labels;
                chart.data.datasets[0].data = r.data.points;
                chart.update();
            });
        self.loadJS("https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.9.3/Chart.min.js")
            .then(()=>self.loadJS("https://cdn.jsdelivr.net/npm/chartjs-plugin-colorschemes"))
            .then(()=>{
                chart = new Chart(ctx, {
                    type: element.dataset.type,
                    data: {
                        datasets: [{
                            data: [],
                            pointRadius: 10
                        }],
                        labels: []
                    },
                    options: {
                        layout: {padding: 5},
                        legend: {display: false},
                        scales: {
                            yAxes: [{
                                ticks: {
                                    suggestedMin: 0,
                                    callback: (value, index, values)=>{
                                        var mins = Math.floor(value / 60) % 60;
                                        var hours = Math.floor((value / 60) / 60);
                                        return ((0<hours)?hours+':':'')
                                            + ((10<mins)?mins:'0'+mins);
                                    }
                                }
                            }]
                        },
                        plugins: {
                            colorschemes: {scheme: 'tableau.Classic20'}
                        }
                    }
                });
                if(element.dataset.type == "bar")
                    chart.options.scales.yAxes[0].ticks.suggestedMax = 1.0;
                refresh();
            });
        [].forEach.call(element.querySelectorAll("select"), (el)=>{
            el.addEventListener("change", refresh);
        });
    }

    loadJS(source){
        var self = this;
        if(!self.loading[source])
            self.loading[source] = new Promise((ok)=>{
                var scripts = document.querySelectorAll("script");
                for(var i=0; i<scripts.length; i++){
                    if(scripts[i].getAttribute("src") == source){
                        ok();
                        return;
                    }
                }
                self.log("Loading", source);
                var el = self.constructElement("script",{
                    attributes: {
                        type: "text/javascript",
                        crossorigin: "anonymous",
                        src: source
                    }
                });
                el.addEventListener("load", ok);
                document.querySelector("body").appendChild(el);
            });
        return self.loading[source];
    }
}

var hourly;
document.addEventListener("DOMContentLoaded", ()=>hourly = hourly || new Hourly());
